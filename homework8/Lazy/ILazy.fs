namespace Lazy

type public ILazy<'a> = 
    abstract member Get: unit -> 'a