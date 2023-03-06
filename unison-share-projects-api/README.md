The Project-related part of the Share API.

Ultimately this package should just be folded into one package that defines the entire Servant API for a
Share-compatible server to implement.

However, that package should (probably) be called `unison-share-api`, which, at the time of writing (Jan 2023),
has a ton of non-Share-API related modules and functionality in it.

So rather than rename it, or rip modules out, or anything like that, we decided to make this new package, which should
be deleted eventually.

Small complication: it was discovered that the projects API needs to depend on the `HashJWT` type, which was previously
in the `unison-share-api`. That type has been pulled down here into a module called `Unison.Share.API.Hash`. So already,
`unison-share-projects-api` is a bit of a misnomer: it's really `unison-share-api-for-real-this-time` ;)
