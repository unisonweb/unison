```ucm
test/main> pull @aryairani/test-almost-empty/main lib.base_latest

  The use of `pull` to install libraries is now deprecated.
  Going forward, you can use
  `lib.install @aryairani/test-almost-empty/main`.

  Downloaded 2 entities.

  I installed @aryairani/test-almost-empty/main as
  aryairani_test_almost_empty_main.

test/main> pull @aryairani/test-almost-empty/main a.b

  Sorry, I wasn’t sure how to process your request. I think
  you're wanting to merge @aryairani/test-almost-empty/main into
  the a.b namespace, but the `pull` command only supports
  merging into the top level of a local project branch.
  
  You can run `help pull` for more information on using `pull`
    The `pull` command merges a remote namespace into a local
    branch
    
    `pull @unison/base/main`                merges the branch
                                            `main` of the Unison
                                            Share hosted project
                                            `@unison/base` into
                                            the current branch
    `pull @unison/base/main my-base/topic`  merges the branch
                                            `main` of the Unison
                                            Share hosted project
                                            `@unison/base` into
                                            the branch `topic`
                                            of the local
                                            `my-base` project
    
    where `remote` is a project or project branch, such as:
      Project (defaults to the /main branch) `@unison/base`
      Project Branch                         `@unison/base/feature`
      Contributor Branch                     `@unison/base/@johnsmith/feature`
      Project Release                        `@unison/base/releases/1.0.0`

test/main> pull @aryairani/test-almost-empty/main a

  I think you're wanting to merge
  @aryairani/test-almost-empty/main into the a branch, but it
  doesn't exist. If you want, you can create it with
  `branch.empty a`, and then `pull` again.

test/main> pull @aryairani/test-almost-empty/main .a

  Sorry, I wasn’t sure how to process your request. I think
  you're wanting to merge @aryairani/test-almost-empty/main into
  the .a namespace, but the `pull` command only supports merging
  into the top level of a local project branch.
  
  You can run `help pull` for more information on using `pull`
    The `pull` command merges a remote namespace into a local
    branch
    
    `pull @unison/base/main`                merges the branch
                                            `main` of the Unison
                                            Share hosted project
                                            `@unison/base` into
                                            the current branch
    `pull @unison/base/main my-base/topic`  merges the branch
                                            `main` of the Unison
                                            Share hosted project
                                            `@unison/base` into
                                            the branch `topic`
                                            of the local
                                            `my-base` project
    
    where `remote` is a project or project branch, such as:
      Project (defaults to the /main branch) `@unison/base`
      Project Branch                         `@unison/base/feature`
      Contributor Branch                     `@unison/base/@johnsmith/feature`
      Project Release                        `@unison/base/releases/1.0.0`

```
