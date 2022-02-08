" Unison functionality for Vim, including type/term omnicompletion.
"
" Maintainer: Unison Computing
" Original Author: Cody Allen (ceedubs)

if exists('g:autoloaded_unison')
  finish
endif
let g:autoloaded_unison = 1

let s:required_config_value = "!REQUIRED!"

" adapted from https://github.com/rust-lang/rust.vim/blob/4aa69b84c8a58fcec6b6dad6fe244b916b1cf830/autoload/rust.vim#L9-L18
function! s:config(name, default) abort
  let name = 'unison_' . a:name
  " Local buffer variable with same name takes predeence over global
  if has_key(b:, name)
    return get(b:, name)
  elseif has_key(g:, name)
    return get(g:, name)
  elseif a:default == s:required_config_value
    throw 'Missing required configuration value: ' . name
  else
    return a:default
  endif
endfunction

function! s:curl_path() abort
  return s:config('curl_path', "curl")
endfunction

function! s:jq_path() abort
  return s:config('jq_path', "jq")
endfunction

function! unison#SetBufferDefaults() abort
  if s:config('set_buffer_defaults', 1)
    " Since Unison completion is fuzzy and not prefix-based, 'longest' doesn't
    " work well, and 'noinsert' behaves a little better.
    setlocal completeopt=menuone,noinsert,preview

    setlocal omnifunc=unison#Complete
  endif
endfunction

" Unison completion satisfying the standard vim completion signature, such
" that it can be assigned to omnifunc.
" vim will first call this to find the base input that should be completed,
" and then will call it again with the base input.
function! unison#Complete(findstart, base) abort
  if a:findstart
    " locate the start of the word
    let line = getline('.')
    let start = col('.') - 1
    " Examples of where we want to count the start of a word:
    "
    " foo List.fold<cursor>
    "     ^
    "
    " {Abor<cursor>
    "  ^
    "
    " (List.fol<cursor>
    "  ^
    while start > 0 && line[start - 1] !~ '\v\s|[(){}\[\]]'
      let start -= 1
    endwhile
    return start
  else
    return unison#CompleteForBase(a:base)
  endif
endfunction

" Return an array of completion items for the provided base input. For example
" base could be 'List.foldL', in which case the top result would probably be
" 'List.foldLeft'.
function! unison#CompleteForBase(base) abort
  let resultLimit = s:config('complete_result_limit', 20)
  let apiHost = s:config('api_host', 'localhost')
  let apiPort = s:config('api_port', s:required_config_value)
  let apiToken = s:config('api_token', s:required_config_value)
  let apiUri = 'http://' . apiHost . ':' . apiPort . '/' . apiToken . '/api/find'

  let curlCommand = s:curl_path() . " -Gfs
        \ --data-urlencode 'limit=" . resultLimit . "'
        \ --data-urlencode 'query=" . a:base . "' "
        \ . apiUri

  let jqFilter = '
        \ def prettyTermType: .termType|[(.[] | .segment)]|add;
        \ def prettyTypeDef: if .tag == "BuiltinObject" then "builtin type " else "" end + (.contents|[(.[] | .segment)]|add);
        \ def termToMatch: {
        \   word: .bestFoundTermName,
        \   info: (.namedTerm.termName + " : " + (.namedTerm|prettyTermType)),
        \   menu: .namedTerm|prettyTermType
        \ };
        \ def typeToMatch: {
        \   word: .bestFoundTypeName,
        \   info: (.namedType.typeName + " : " + (.typeDef|prettyTypeDef)),
        \   menu: .typeDef|prettyTypeDef
        \ };
        \ .[][1]|(
        \   (select(.tag == "FoundTermResult")|.contents|termToMatch),
        \   (select(.tag == "FoundTypeResult")|.contents|typeToMatch)
        \ )'

  let command = curlCommand . " | " . s:jq_path() . " -c '" . jqFilter . "'"
  let lines = system(command)
  let resultObjects = split(lines, "\n")
  call map(resultObjects, {_, val -> json_decode(val)})
  return resultObjects
endfunction

" vim: set et sw=2 sts=2 ts=2:
