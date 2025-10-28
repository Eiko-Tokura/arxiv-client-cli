#!/bin/bash

cat << 'EOF' > README.md
## Arxiv Cli

`arxiv-client-cli` is a command-line interface (CLI) tool for searching and downloading research papers from arXiv.org, it warps the library `arxiv-client`.

### Help Text

```
EOF

arxiv-client-cli --help >> README.md

echo '```' >> README.md
echo '### Constructing Query String' >> README.md
echo '```' >> README.md

arxiv-client-cli --helpQuery >> README.md

cat << 'EOF' >> README.md
```

### Installation

```bash
cabal install arxiv-client-cli
```
EOF
