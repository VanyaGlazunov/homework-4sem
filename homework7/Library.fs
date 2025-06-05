namespace homework7

open System.Net.Http
open System.Text.RegularExpressions
open System.Threading
open System

module MiniCrawler =
    /// Contains parameters for crawl.
    type CrawlConfig = {
        Retries: int
        InitialDelayMs: int
        MaxParallelDownloads: int
        ct: CancellationToken
    } with
        member c.validate() =
            if c.Retries <= 0 then
                invalidArg "Retries" "You should try at least once."
            if c.InitialDelayMs < 0 then
                invalidArg "InitialDelayMs" "Delay time should be non-negative"

    let defaultCrawlerConfig = {
        Retries = 3
        InitialDelayMs = 100
        MaxParallelDownloads = 6
        ct = CancellationToken.None
    }

    /// Downloads page by url with retries.
    let rec downloadPageContent 
        (client: HttpClient)
        (url: string)
        (config: CrawlConfig)
        = 
        config.validate()
        async {
            let operation = 
                async {
                    let! content = client.GetStringAsync (url, config.ct) |> Async.AwaitTask
                    return content
                }
            
            if config.Retries = 1 then
                    return! operation
            else
                let! result = operation |> Async.Catch
                match result with
                | Choice1Of2 content -> return content
                | Choice2Of2 ex ->
                    match ex with
                    | :? HttpRequestException ->
                        do! Async.Sleep config.InitialDelayMs
                        return! downloadPageContent client  url config
                    | _ -> return raise ex

        }
    
    /// Extracts links from page content that look like.
    let extractLinks (content: string) = 
        let regex = Regex @"<a href=""(http://[^""]+)"""

        regex.Matches content
        |> Seq.cast<Match>
        |> Seq.map (fun m -> m.Groups.[1].Value)
        |> List.ofSeq

    /// Represents status of page download.
    type PageDownloadResult = 
    | Success of url: string * size: int
    | Failure of url: string * error: string


    /// Type that can crawl web pages.
    type Crawler(client: HttpClient) = 
        member private c.crawlInternal url (config: CrawlConfig) = 
            async {
                let! content = downloadPageContent client url config

                let links = extractLinks content

                let downloadTasks =
                    links
                    |> List.map(fun url -> 
                    async {
                        let operation = 
                            async {
                                let! page = downloadPageContent client url config
                                return Success (url, page.Length)
                            }
                        
                        let! result = operation |> Async.Catch

                        match result with
                        | Choice1Of2 r -> return r
                        | Choice2Of2 ex ->
                            match ex with 
                            | :? OperationCanceledException -> return raise ex
                            | _ -> return Failure (url, ex.Message)
                    })

            
                return! Async.Parallel(downloadTasks, maxDegreeOfParallelism = config.MaxParallelDownloads)
            }

        /// Crawls page from given url. Throws exception when page can't be downloaded.
        member public c.crawl url = 
            c.crawlInternal url defaultCrawlerConfig

        /// Crawls page from given url with specified CrawlConfig. Throws exception when page can't be downloaded.
        member public c.crawlWithConfig url config =
            c.crawlInternal url config
    
