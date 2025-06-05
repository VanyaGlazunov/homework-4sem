module Tests

open NUnit.Framework
open FsUnit
open homework7.MiniCrawler
open RichardSzalay.MockHttp
open System.Net
open System.Net.Http
open System

let accepted = 
    Map [
        "http://main.com", "<a href=\"http://child1.com\"><a href=\"http://child2.com\">"
        "http://child1.com", "Content1"
        "http://child2.com", "Longer Content2"
    ]

let bad = 
    [
        "http://bad.com"
        "http://timeout.com"
    ]

let mockHttp () =
    let mock = new MockHttpMessageHandler ()
    for KeyValue(url, content) in accepted do
        mock.When(url).Respond(HttpStatusCode.Accepted, new StringContent(content)) |> ignore

    let mutable attempt = 0
    mock.When("http://retry.com").Respond(fun _ -> 
        attempt <- attempt + 1    
        if attempt < 2 then
            new HttpResponseMessage(HttpStatusCode.BadRequest)
        else
            new HttpResponseMessage(Content = new StringContent "success")
    ) |> ignore
    
    for url in bad do
        mock.When(url).Respond HttpStatusCode.BadRequest |> ignore

    mock.ToHttpClient()

[<Test>]
let ``extractLinks should parse absolute HTTP links correctly``() =
    let html = """
        <a href="http://example.com/page1">Link1</a>
        <a href="http://test.site/page2">Link2</a>
    """
    extractLinks html
    |> should equal [ "http://example.com/page1"; "http://test.site/page2" ]

[<Test>]
let ``extractLinks should ignore non-http links``() =
    let html = """
        <a href="/relative">Invalid</a>
        <a href="ftp://example.com">FTP</a>
    """
    extractLinks html
    |> should be Empty

[<Test>]
let ``downloadPageContent returns page content`` () = 
    let client = mockHttp ()

    for KeyValue(url, content) in accepted do
        let actual = downloadPageContent client url defaultCrawlerConfig |> Async.RunSynchronously
        actual |> should equal content

[<Test>]
let ``downloadPageContent returns exceptions`` () = 
    let client = mockHttp ()

    for url in bad do
        (fun () ->
    downloadPageContent client url defaultCrawlerConfig
        |> Async.RunSynchronously
        |> ignore)
        |>  should throw typeof<AggregateException> |> ignore 
    
[<Test>]
let ``Crawl success`` () =
    let client = mockHttp ()
    let crawler = Crawler client

    let actual = crawler.crawl "http://main.com" |> Async.RunSynchronously |> Array.toList
    let expected = [Success ("http://child1.com", 8); Success ("http://child2.com", 15)]

    actual |> should equal expected