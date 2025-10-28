## Arxiv Cli

`arxiv-client-cli` is a command-line interface (CLI) tool for searching and downloading research papers from arXiv.org, it warps the library `arxiv-client`.

### Help Text

```
Arxiv Client CLI

Usage: arxiv-client-cli [-q|--query TEXT] [-H|--helpQuery] [-p|--downloadPdf] 
                        [-s|--downloadSrc] [-u|--ungzip] [-c|--concise] 
                        [-v|--detail] [-a|--abstract] [-j|--json] 
                        [-d|--downloadDir STRING] [--after yyyy-mm-dd] 
                        [--before yyyy-mm-dd] [-m|--maxResult INT] 
                        [-n|--page INT]

Available options:
  -h,--help                Show this help text
  -q,--query TEXT          Search query string
  -H,--helpQuery           Display help for constructing query strings
  -p,--downloadPdf         Download PDF files for each entry
  -s,--downloadSrc         Download source .tar.gz files for each entry
  -u,--ungzip              Automatically ungzip source files (by running "tar
                           -xzf")
  -c,--concise             Concise output (only titles)
  -v,--detail              Detailed output
  -a,--abstract            Include abstracts in the output
  -j,--json                Output results in JSON format, turn off
                           human-readable output
  -d,--downloadDir STRING  Directory path to save downloaded files
                           (default: "./")
  --after yyyy-mm-dd       Only include papers published after this date
                           (YYYY-MM-DD)
  --before yyyy-mm-dd      Only include papers published before this date
                           (YYYY-MM-DD)
  -m,--maxResult INT       Maximum results per page (default: 25)
  -n,--page INT            Page number to retrieve (starting from 0)
```
### Constructing Query String
```
--query parameter is mandatory.
Query string is constructed by using

* <field> <match> <value>
  <field> := title | author | abstract | category | anywhere
  (<match>, <value>) :=
    is <string>          -- exact match
    has <string>         -- substring match
    any [<string>, ...]  -- any of the strings (ors)
    all [<string>, ...]  -- all of the strings (ands)

* You can write a single string "value" without specifying field and match,
  which is equivalent to: anywhere has "value"

* Logical operators:
  &&      -- and (note that these operators are right associative, use brackets for clarity)
  ||      -- or  (note that these operators are right associative, use brackets for clarity)
  ands [<queryTerm>, ...]  -- and multiple terms
  ors  [<queryTerm>, ...]  -- or  multiple terms
  not <queryTerm>          -- negate term

Examples:
  --query 'title has "quantum" && author is "Albert Einstein"'
  --query 'author any ["john doe", "jane smith"]'
  --query 'ands [title is "coleman", author has "doe"]'
  --query 'ors [category is "math.NT", category is "math.AG"]'
```

### Installation

```bash
cabal install arxiv-client-cli
```
