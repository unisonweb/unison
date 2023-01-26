The Project-related part of the Share API.

Ultimately this package should just be folded into one package that defines the entire Servant API for a
Share-compatible server to implement.

However, that package should (probably) be called `unison-share-api`, which, at the time of writing (Jan 2023),
has a ton of non-Share-API related modules and functionality in it.

So rather than rename it, or rip modules out, or anything like that, we decided to make this new package, which should
be deleted eventually.
