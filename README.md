# feed-parser
small feed parsing library for hoon

all code in https://github.com/nogira/feed-parser/blob/main/desk/lib/feed-parse.hoon

- use like so
```hoon
/+  feed-parse
=,  feed-parse
...
(parse-feed feed-cord)  :: feed-cord is string of rss/atom/json
```