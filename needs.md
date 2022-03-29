# https://github.com/unisonweb/unison/issues/2110

* Allow querying for the names of a set of references/referents within the scope of a given branch.
  * Useful when 'use' statements bring branches into scope
  * Backend uses this for something?
  * Can be used to look up names within the root namespace using the ".> names #abcdef" command.

* Allow querying for the hashes which match a given "name", where a name is either a full path, or a partial suffix, used for parsing.

---

* Efficiently look up old names.
  * When a reference is orphaned we still want to be able to display _some_ name for it, we should be able to query our root branch's history to efficiently find old names for a given ref.


* Lazily load a sub-namespace by path.
    * Currently the only way to find our way to a given child branch is to parse and load the entire root namespace :| 
    * How does this interact with the codebase server?
    * Could we be more lazy if we only allowed push/pull on _projects_


<!-- How do we track the empty namespace? I suppose it's just the hash with no joins in the namespace tables? -->

<!-- Queries need to be made relative to a specific namespace, since given a namespace hash I don't actually know where it's mounted within the root namespace, it might be mounted at several spots. -->
