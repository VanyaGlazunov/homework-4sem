namespace Lazy

/// Represents lazy computaion.
type public ILazy<'a> = 
    abstract member Get: unit -> 'a