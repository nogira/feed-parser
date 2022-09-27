:: this lib is based on https://github.com/feed-rs/feed-rs (fave feed lib !!)
:: atom specs: https://validator.w3.org/feed/docs/atom.html
=<
|%
+$  category
  $:  label=(unit @t)
      scheme=(unit @t)
      term=@t
  ==
+$  content
  $:  body=(unit @t)
      content-type=@t  :: should be mime, see $+text for more details
      length=(unit @ud)
      src=(unit link)
  ==
+$  entry
  $:  authors=(list person)
      categories=(list category)
      content=(unit content)
      contributors=(list person)
      id=@t
      links=(list link)
      media=(list media-object)
      published=(unit @da)
      rights=(unit text)
      source=(unit @t)
      summary=(unit text)
      title=(unit text)
      updated=(unit @da)
  ==
+$  feed
  $:  authors=(list person)
      categories=(list category)
      contributors=(list person)
      description=(unit text)
      entries=(list entry)
      feed-type=?(%rss-2 %atom %json)
      generator=(unit generator)
      icon=(unit image)
      id=@t
      language=(unit @t)
      links=(list link)
      logo=(unit image)
      published=(unit @da)  :: date in UTC timezone
      rating=(unit media-rating)
      rights=(unit text)
      title=(unit text)
      ttl=(unit @ud)
      updated=(unit @da)
  ==
+$  generator
  $:  content=@t
      uri=(unit @t)
      version=(unit @t)
  ==
+$  image
  $:  description=(unit @t)
      height=(unit @ud)
      link=(unit link)
      title=(unit @t)
      uri=@t
      width=(unit @ud)
  ==
+$  link
  $:  href=@t
      href-lang=(unit @t)
      length=(unit @ud)
      media-type=(unit @t)
      rel=(unit @t)
      title=(unit @t)
  ==
+$  media-community
  $:  stars-avg=(unit @rs)
      stars-count=(unit @ud)
      stars-min=(unit @ud)
      stars-max=(unit @ud)
      stats-favorites=(unit @ud)
      stats-views=(unit @ud)
  ==
+$  media-content
  $:  content-type=(unit @t)  :: should be (unit mime), see $+text for more details
      duration=(unit @dr)
      height=(unit @ud)
      rating=(unit media-rating)
      size=(unit @ud)
      url=(unit @t)
      width=(unit @ud)
  ==
+$  media-credit  entity=@t
+$  media-object
  $:  community=(unit media-community)
      content=(list media-content)
      credits=(list media-credit)
      description=(unit text)
      duration=(unit @dr)
      texts=(list media-text)
      thumbnails=(list media-thumbnail)
      title=(unit text)
  ==
+$  media-rating
  $:  urn=@t
      value=@t
  ==
+$  media-text
  $:  end-time=(unit @dr)
      start-time=(unit @dr)
      =text
  ==
+$  media-thumbnail
  $:  =image
      time=(unit @dr)
  ==
+$  person
  $:  avatar=(unit @t)
      email=(unit @t)
      name=@t
      uri=(unit @t)
  ==
+$  text
  $:  content=@t
      :: feed-rs uses type `mime` for  `content-type`, but idk how to do enums 
      :: with default vals, so just doing str instead
      :: if figure out, here is the mimes data type to copy:
      :: https://docs.rs/mime/0.3.16/mime/index.html
      content-type=@t
      src=(unit @t)
  ==
::
++  parse-feed
  =<
  |=  feed-str=@t
  ^-  feed
  :: get first char of feed `cord` to determine is json or xml
  ?:  =(`@t`(cut 3 [0 1] feed-str) '{')
    (json-parse feed-str)
  (xml-parse feed-str)
  |%
  ++  json-parse
    =<
    |=  feed-str=@t
    ^-  feed
    =/  j  (need (de-json:html feed-str))  :: if crash, good :)
    =*  inp  (get-obj j)
    =|  out=feed
    :: authors
    =.  authors.out  (p-authors inp)
    :: categories
    ::   - non-existent
    :: contributors
    ::   - non-existent
    :: description
    =/  desc  (get-str-attr inp 'description')
    =.  description.out  ?~  desc
        ~
      [~ content=(need desc) content-type='text' src=~]
    :: entries
    =.  entries.out  (p-entries inp)
    :: feed-type
    =.  feed-type.out  %json
    :: generator
    ::   - non-existent
    :: icon
    =/  icon  (get-str-attr inp 'icon')
    =.  icon.out  ?~  icon
        ~
      [~ [description=~ height=~ link=~ title=~ uri=(need icon) width=~]]
    :: id
    =.  id.out  (need (get-str-attr inp 'feed_url'))
    :: language
    =.  language.out  (get-str-attr inp 'language')
    :: links
    =.  links.out  (p-links inp)
    :: logo
    =/  fi  (get-str-attr inp 'favicon')
    =.  logo.out  ?~  fi
        ~
      [~ [description=~ height=~ link=~ title=~ uri=(need fi) width=~]]
    :: published
    ::   - non-existent
    :: rating
    ::   - non-existent
    :: rights
    ::   - non-existent
    :: title (mandatory val so no need to handle optional)
    =.  title.out  [~ content=(need (get-str-attr inp 'title')) content-type='text' src=~]
    :: ttl
    ::   - non-existent
    :: updated
    ::   - non-existent
    ::
    :: =/  obj-val  [[p='version' q=[%o p=[p='version' q=[%s p='https://jsonfeed.org/version/1']]]]]
    :: ~&  (get-obj-attr obj-val 'version')
    out
    ::
    :: --parsing the diff types--
    =<
    |%
    ++  p-links
      |=  mp=(map @t json)
      ^-  (list link)
      =/  attr-names  (limo ~['home_page_url' 'feed_url' 'next_url'])
      =/  urls  %+  turn  attr-names
        |=  at=@t
        =/  v  (get-str-attr mp at)
        ?~  v
          ~
        (need v)
      :: rm nulls
      =/  urls  `(list @t)``(list @)`(skim urls |*(url=* ?!(?=(%~ url))))
      (turn urls |=(a=@t [href=a href-lang=~ length=~ media-type=~ rel=~ title=~]))
    ++  p-authors
      |=  mp=(map @t json)
      ^-  (list person)
      =/  arr  (get-arr-attr mp 'authors')
      %+  turn  arr
        |*  a=*
        =|  author=person
        =/  mp  (get-obj a)
        =.  email.author  ~
        =.  avatar.author  (get-str-attr mp 'avatar')
        =.  uri.author  (get-str-attr mp 'url')
        =.  name.author  (need (get-str-attr mp 'name'))
        author
    :: convert @t of json feed to @da
    ++  p-entries
      =<
      :: input object value
      |=  mp=(map @t json)
      ^-  (list entry)
      =/  arr  (get-arr-attr mp 'items')
      =|  entries=(list entry)
      :: map over entries
      %+  turn  arr
        |*  a=*
        =|  entry=entry
        =/  mp  (get-obj a)
        :: authors
        =.  authors.entry  (p-authors mp)
        :: categories
        =/  arr  (get-arr-attr mp 'tags')
        =.  categories.entry  ?~  arr
            ~
          =/  tgs  %+  turn  arr
            |*  a=*
            ^-  @t
            ?:  ?=([%s p=@t] a)
              p:a
            ''
          (turn tgs |=(tg=@t [label=[~ tg] scheme=~ term=tg]))
        :: content
        =/  cnt  (get-str-attr mp 'content_html')
        =/  src  (get-str-attr mp 'url')
        =/  lnk  ?~  src
            ~
          [~ href=(need src) href-lang=~ length=~ media-type=~ rel=~ title=~]
        =.  content.entry  ?~  cnt
            :: if null
            =/  cnt  (get-str-attr mp 'content_text')
            [~ body=cnt content-type='text' length=~ src=lnk]
          :: if present
          [~ body=cnt content-type='html' length=~ src=lnk]
        :: contributors
        ::   - non-existent
        :: id
        =.  id.entry  (need (get-str-attr mp 'id'))
        :: links
        =/  lnk  (get-str-attr mp 'external_url')
        =.  links.entry  ?~  lnk
            ~
          %-  limo
          ~[[href=(need lnk) href-lang=~ length=~ media-type=~ rel=~ title=~]]
        :: media
        =.  media.entry  (p-media mp)
        :: published 
        =.  published.entry  (p-date (get-str-attr mp 'date_published'))
        :: rights
        ::   - non-existent
        :: source
        =.  source.entry  src
        :: summary
        =/  sm  (get-str-attr mp 'summary')
        =.  summary.entry  ?~  sm
            ~
          [~ content=(need sm) content-type='text' src=~]
        :: title
        =/  tt  (get-str-attr mp 'title')
        =.  title.entry  ?~  tt
            ~
          [~ content=(need tt) content-type='text' src=~]
        :: updated
        =.  updated.entry  (p-date (get-str-attr mp 'date_modified'))
        ::
        entry
      ::
      |%
      ++  p-media
        |=  mp=(map @t json)
        ^-  (list media-object)
        :: attachments
        =/  arr  (get-arr-attr mp 'attachments')
        =/  lm=(list media-object)  ?~  arr
            ~
          :: convert list of attachments to list of media-object
          =/  ams  %+  turn  arr
            |*  el=*
            ^-  media-object
            =/  mp  (get-obj el)
            =/  url  (get-str-attr mp 'url')
            =/  mim  (get-str-attr mp 'mime_type')
            =/  tt  (get-str-attr mp 'title')
            =/  ttt  ?~  tt
                ~
              [~ content=(need tt) content-type='text' src=~]
            =/  sz  (get-num-attr mp 'size_in_bytes')
            =/  dr  (get-num-attr mp 'duration_in_seconds')
            =/  dur  ?~  dr
                ~
              [~ `@dr`(yule `tarp`[0 0 0 (need dr) ~])]
            ~&  *media-content
            =/  cont=(list media-content)  (limo ~[[content-type=mim duration=dur height=~ rating=~ size=sz url=url width=~]])
            =/  mob=media-object  [community=~ content=cont credits=~ description=~ duration=~ texts=~ thumbnails=~ title=ttt]
            mob
          ::
          ams
        ::
        :: main image
        =/  im  (get-str-attr mp 'image')
        :: append if present
        =.  lm  ?~  im
            lm
          =/  cont  (limo ~[[content-type=[~ 'image'] duration=~ height=~ rating=~ size=~ url=im width=~]])
          =/  mob  [community=~ content=cont credits=~ description=~ duration=~ texts=~ thumbnails=~ title=~]
          [mob lm]
        :: banner image
        =/  im  (get-str-attr mp 'banner_image')
        :: append if present
        =.  lm  ?~  im
            lm
          =/  cont  (limo ~[[content-type=[~ 'image'] duration=~ height=~ rating=~ size=~ url=im width=~]])
          =/  mob  [community=~ content=cont credits=~ description=~ duration=~ texts=~ thumbnails=~ title=~]
          [mob lm]
        ::
        lm
      --
    --
    :: --manual json parsing helper functions--
    |%
    :: get `*` from `[%o *]`
    ++  get-obj
      |=  obj=json
      ^-  (map @t json)
      ?>  ?=([%o p=*] obj)
      p:obj
    :: get object value using key on object
    :: ++  get-obj-attr
    ::   |*  [obj-val=(map @t json) key=@t]
    ::   :: ^-  (map @t json)
    ::   =/  attr  (need (~(get by obj-val) key))
    ::   ?>  ?=([%o p=*] attr)  :: for some reason i need to add `p=` to be able 
    ::   p:attr                  :: to find p for `%s`, but not for `%o`
    :: :: get string value using key on object
    :: get array value using key on object
    ++  get-arr-attr
      |=  [mp=(map @t json) key=@t]
      ^-  (list json)
      =/  attr  (~(get by mp) key)
      :: if null, return null
      ?~  attr
        ~
      =/  attr  (need attr)
      ?>  ?=([%a p=*] attr)  :: for some reason i need to add `p=` to be able 
      p:attr                 :: to find p for `%s` and `%a`, but not for `%o`
    :: get string value using key on object
    ++  get-str-attr
      |=  [mp=(map @t json) key=@t]
      ^-  (unit @t)
      =/  attr  (~(get by mp) key)
      :: if null, return null
      ?~  attr
        ~
      =/  attr  (need attr)
      ?>  ?=([%s p=@t] attr)  :: for some reason i need to add `p=` to be able 
      [~ p:attr]              :: to find p for `%s`, but not for `%o`
    :: get number value using key on object
    ++  get-num-attr
      |=  [mp=(map @t json) key=@t]
      ^-  (unit @ud)
      =/  attr  (~(get by mp) key)
      :: if null, return null
      ?~  attr
        ~
      =/  attr  (need attr)
      ?>  ?=([%n p=@ta] attr)  :: json numbers are knots bc reasons
      :: [~ p:attr]
      [~ (ni:dejs:format attr)]
    --
  ::
  ++  xml-parse
    =<
    |=  feed-str=@t
    ^-  feed
    =|  out=feed
    =/  xml  (need (de-xml:html feed-str))
    =/  bcl  c:xml  :: list of children in base
    :: loop through each xml base-element, with a switch statement processing 
    :: each element. including other arms that e.g. does the processing 
    :: (looping through elements/attributes) for an author, etc
    ?:  =(n:g:xml %feed)  :: if this is %feed, it's an atom feed, else it's a rss feed
        =.  feed-type.out  %atom
        (p-atom bcl out)
      =.  feed-type.out  %rss-2
      (p-rss bcl out)
    =<
    |%  :: --rss parsing--
    ++  p-rss
      |=  [bcl=(list manx) out=feed]
      ^-  feed
      |-
      :: if list (`t` face) is null, is end of list, so return list
      ?~  bcl
        out
      =/  tag  n:g:i:bcl
      ~&  tag  :: print element tag
      ::
      =.  out  ?+  tag  ~&  "TAG NOT FOUND: {<tag>}"  out
          %item  out
        ==
      $(bcl t:bcl, out out)
    --
    =<
    |%  :: --atom parsing--
    ++  p-atom
      |=  [bcl=(list manx) out=feed]
      ^-  feed
      |-
      :: if list (`t` face) is null, is end of list, so return list
      ?~  bcl
        out
      =/  elem  i:bcl
      =/  tag  n:g:elem
      ~&  tag  :: print element tag
      ::
      =.  out  ?+  tag  ~&  "TAG NOT FOUND: {<tag>}"  out
          %author  out
          %entry  out
          %id   =.  id.out  (inner-txt elem)  out
          %link  =.  links.out  (p-link elem links.out)  out
          %published  out
          %title  =.  title.out  [~ content=(inner-txt elem) content-type='text' src=~]  out
        ==
      $(bcl t:bcl, out out)
    --
    |%  :: --xml parsing--
    ++  inner-txt
      |=  el=manx
      ^-  @t
      (crip v:i:&1:a:g:i:&1:c:el)
    ++  p-link
      |=  [el=manx lks=(list link)]
      ^-  (list link)
      =/  attrs  a:g:el
      ~&  attrs
      :-
      %+  roll  attrs
        :: it seems for <link> it never uses `[@tas @tas]` from the type union, 
        :: but needs to be there anyway for the type checker
        |=  [[term=?(@tas [@tas @tas]) val=tape] accum=link]
        ^-  link
        ?+  term  !!  :: should never crash
          %href  =.  href.accum  (crip val)  accum
          %href-lang  =.  href-lang.accum  [~ (crip val)]  accum
          %length  =.  length.accum  [~ (scan (trip (crip val)) dem)]  accum
          %media-type  =.  media-type.accum  [~ (crip val)]  accum
          %rel  =.  rel.accum  [~ (crip val)]  accum
          %title  =.  title.accum  [~ (crip val)]  accum
        ==
      lks
    --
  --
--
:: --general helper functions--
|%
++  p-date
  =<
  |=  t=(unit @t)
  ^-  (unit @da)
  :: return null if input is null
  ?~  t
    ~
  =/  t  (need t)
  :: time format: https://www.rfc-editor.org/rfc/rfc3339
  :: e.g. '2020-08-07T11:44:36-05:00'
  :: y:m:d
  =+  yr=`@t`(cut 3 [0 4] t)
  =+  mo=(cut-2 t 5)
  =+  dy=(cut-2 t 8)
  :: h:m:s
  =+  hr=(cut-2 t 11)
  =+  mn=(cut-2 t 14)
  =+  sc=(cut-2 t 17)
  :: timezone (account for timezone)
  =/  df=@dr  ?:  =(`@t`(cut 3 [19 1] t) 'Z')  :: if no timezone, no need to change
                ~m0  :: 0 min
              =+  hr-df=`@dr`(yule `tarp`[0 (num (cut-2 t 20)) 0 0 ~])
              =+  mn-df=`@dr`(yule `tarp`[0 0 (num (cut-2 t 23)) 0 ~])
              `@dr`(add hr-df mn-df)
  :: check if positive or negative diff
  ::   from time format link above:
  ::     > 1990-12-31T15:59:60-08:00
  ::     > This represents the same leap second in Pacific Standard Time, 8
  ::     > hours behind UTC.
  ::   thus, `-` diff requires adding the @dr to @da to acheive UTC
  =*  df-op   ?:  =(`@t`(cut 3 [19 1] t) '+')
                sub
              add
  :: return as unit
  :-  ~
  ^-  @da  ^-  @
  :: add/subtract timezone diff (@dr) to/from datetime (@da)
  %+  df-op
    :: create @da
    %-  year
    :-  [a=%.y y=(num yr)]
    :-  m=(num mo)
    t=[d=(num dy) h=(num hr) m=(num mn) s=(num sc) f=~]
  df
  ::
  |%
  :: convert @t to @ud
  ++  num
    |=  in=@t
    ^-  @ud
    (scan (trip in) dem)
  :: cut 2 digit @t string from larger @t string
  ++  cut-2
    |=  [a=@t i=@ud]
    `@t`(cut 3 [i 2] a)
  --
--
