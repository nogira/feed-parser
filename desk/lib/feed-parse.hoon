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
+$  media-credit
  $:  content=@t
      role=(unit @t)
      scheme=(unit @t)
  ==
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
    :: de-xml:html seems to generally require just 1 root element (like reactjs)
    :: , and while it can parse '<xml /><feed></feed> fine for some reason, it 
    :: cannot parse things like '<xml /><xml-stylesheet /><rss></rss>', thus 
    :: better to just remove all elements starting with '<xml' from cord prior 
    :: to parsing the xml to manx
    =/  i  0
    =/  rdy-for-tag  |
    =/  curr-tag  ""
    =/  feed-str  |-
    =/  char  `@t`(cut 3 [i 1] feed-str)
    :: if char is '<'
    ?:  =(char '<')
      $(i +(i), rdy-for-tag &)
    :: if not ready for tag collection, go next loop
    ?.  rdy-for-tag
      $(i +(i))
    :: when tag is 3 long, check it matches "xml"
    ?:  =((lent curr-tag) 4)
      ?:  ?!(=(curr-tag "?xml"))
        :: if doesn't match "?xml", end and return xml w/o all chars prev to "<123"
        =/  start-idx  (sub i 5)
        =/  n-chars  (met 3 feed-str) :: get length of `feed-str` cord
        =.  n-chars  (sub n-chars start-idx)
        `@t`(cut 3 [start-idx n-chars] feed-str)
      :: if matches, reset and wait for next tag
      $(i +(i), rdy-for-tag |, curr-tag "")
    :: add char to curr-tag
    $(i +(i), curr-tag (weld curr-tag ~[char]))
    ::
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
      :: bcl currently list of 1 channel element, so must get channels' children
      ?~  bcl  :: this is neccessary to access `i`
        out
      =/  bcl  c:i:bcl
      ::
      |-
      :: if list (`t` face) is null, is end of list, so return list
      ?~  bcl
        out
      =/  tag  n:g:i:bcl
      ~&  tag  :: print element tag
      :: ~&  n:g:i:&1:c:i:bcl
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
          %author  =.  authors.out  (p-person elem authors.out)  out
          %category  =.  categories.out  (p-category elem categories.out)  out
          %contributor  =.  contributors.out  (p-person elem contributors.out)  out
          %entry  =.  entries.out  (p-entry elem entries.out)  out
          %generator  =.  generator.out  (p-generator elem)  out
          %icon  =/  img  *image
            =.  uri.img  (need (inner-txt elem))
            =.  icon.out  [~ img]  out
          %id  =.  id.out  (need (inner-txt elem))  out
          %link  =.  links.out  (p-link elem links.out)  out
          %logo  =/  img  *image
            =.  uri.img  (need (inner-txt elem))
            =.  logo.out  [~ img]  out
          %published  =.  published.out  (p-date (inner-txt elem))  out
          %rights  =.  rights.out  [~ *text]  out
          %subtitle  =.  description.out  (p-text elem)  out
          %title  =.  title.out  (p-text elem)  out
          %updated  =.  updated.out  (p-date (inner-txt elem))  out
        ==
      $(bcl t:bcl, out out)
    --
    |%  :: --xml parsing--  :: TODO: might need to convert all html entities in xml inner text !!
    ++  inner-txt
      |=  el=manx
      ^-  (unit @t)
      [~ (crip v:i:&1:a:g:i:&1:c:el)]
    ++  p-text
      |=  el=manx
      ^-  (unit text)
      =/  attrs  a:g:el
      =/  out  %+  roll  attrs
        :: check ++p-link for info about `[@tas @tas]`
        |=  [[tag=?(@tas [@tas @tas]) val=tape] accum=text]
        ^-  text
        =/  txt  (crip val)
        ?+  tag  !!  :: should never crash
          %type  =.  content-type.accum  txt  accum
        ==
      =.  content.out  (need (inner-txt el))
      [~ out]
    ++  p-link
      |=  [el=manx lks=(list link)]
      ^-  (list link)
      =/  attrs  a:g:el
      :-
      %+  roll  attrs
        :: it seems for <link> it never uses `[@tas @tas]` from the type union, 
        :: but needs to be there anyway for the type checker
        :: [@tas @tas] is for e.g. <media:group> which convs to [%media %group]
        |=  [[tag=?(@tas [@tas @tas]) val=tape] accum=link]
        ^-  link
        =/  txt  [~ (crip val)]
        ?+  tag  !!  :: should never crash
          %href  =.  href.accum  (need txt)  accum
          %href-lang  =.  href-lang.accum  txt  accum
          %length  =.  length.accum  [~ (scan (trip (need txt)) dem)]  accum
          %media-type  =.  media-type.accum  txt  accum
          %rel  =.  rel.accum  txt  accum
          %title  =.  title.accum  txt  accum
        ==
      lks
    ++  p-person
      |=  [el=manx ppl=(list person)]
      ^-  (list person)
      =/  elems  c:el
      :-
      %+  roll  elems
        :: check ++p-link for info about `[@tas @tas]`
        |=  [elem=manx accum=person]
        ^-  person
        =/  tag  n:g:elem
        =/  txt  (inner-txt elem)
        ?+  tag  !!  :: should never crash
          %avatar  =.  avatar.accum  txt  accum
          %email  =.  email.accum  txt  accum
          %name  =.  name.accum  (need txt)  accum
          %uri  =.  uri.accum  txt  accum
        ==
      ppl
    ++  p-source
      |=  el=manx
      ^-  (unit @t)
      =/  elems  c:el
      %+  roll  elems
        :: check ++p-link for info about `[@tas @tas]`
        |=  [elem=manx accum=(unit @t)]
        ^-  (unit @t)
        =/  tag  n:g:elem
        ?+  tag  !!  :: should never crash
          %id  (inner-txt elem)
        ==
    ++  p-category
      |=  [el=manx cts=(list category)]
      ^-  (list category)
      =/  attrs  a:g:el
      :-
      %+  roll  attrs
        :: check ++p-link for info about `[@tas @tas]`
        |=  [[tag=?(@tas [@tas @tas]) val=tape] accum=category]
        ^-  category
        =/  txt  [~ (crip val)]
        ?+  tag  !!  :: should never crash
          %term  =.  term.accum  (need txt)  accum
          %scheme  =.  scheme.accum  txt  accum
          %label  =.  label.accum  txt  accum
        ==
      cts
    ++  p-generator
      |=  el=manx
      ^-  (unit generator)
      =/  attrs  a:g:el
      =/  out  %+  roll  attrs
        :: check ++p-link for info about `[@tas @tas]`
        |=  [[tag=?(@tas [@tas @tas]) val=tape] accum=generator]
        ^-  generator
        =/  txt  [~ (crip val)]
        ?+  tag  !!  :: should never crash
          %uri  =.  uri.accum  txt  accum
          %version  =.  version.accum  txt  accum
        ==
      =.  content.out  (need (inner-txt el))
      [~ out]
    ++  p-entry
      |=  [el=manx ets=(list entry)]
      ^-  (list entry)
      =/  elems  c:el
      :-
      %+  roll  elems
        :: check ++p-link for info about `[@tas @tas]`
        |=  [elem=manx accum=entry]
        ^-  entry
        =/  tag  n:g:elem
        ?+  tag  ~&  "ENTRY TAG NOT FOUND: {<tag>}"  accum
          %author  =.  authors.accum  (p-person elem authors.accum)  accum
          %category  =.  categories.accum  (p-category elem categories.accum)  accum
          %content  =.  content.accum  (p-content elem)  accum
          %contributor  =.  contributors.accum  (p-person elem contributors.accum)  accum
          %id  =.  id.accum  (need (inner-txt elem))  accum
          %link  =.  links.accum  (p-link elem links.accum)  accum
          [%media %group]  =.  media.accum  (p-media-group elem media.accum)  accum
          %published  =.  published.accum  (p-date (inner-txt elem))  accum
          %rights  =.  rights.accum  (p-text elem)  accum
          %source  =.  source.accum  (p-source elem)  accum
          %summary  =.  summary.accum  (p-text elem)  accum
          %title  =.  title.accum  (p-text elem)  accum
          %updated  =.  updated.accum  (p-date (inner-txt elem))  accum
        ==
      ets
    ++  p-content
      |=  el=manx
      ^-  (unit content)
      =/  attrs  a:g:el
      =/  out  %+  roll  attrs
        :: check ++p-link for info about `[@tas @tas]`
        |=  [[tag=?(@tas [@tas @tas]) val=tape] accum=content]
        ^-  content
        =/  txt  (crip val)
        ?+  tag  !!  :: should never crash
          %content-type  =.  content-type.accum  txt  accum
          %length  =.  length.accum  [~ (scan (trip txt) dem)]  accum
          %src  =/  lnk  *link
            =.  href.lnk  txt
            =.  src.accum  [~ lnk]  accum
        ==
      =.  body.out  (inner-txt el)
      [~ out]
    ++  p-media-group
      =<
      |=  [el=manx mol=(list media-object)]
      ^-  (list media-object)
      =/  elems  c:el
      :-
      %+  roll  elems
        |=  [elem=manx accum=media-object]
        ^-  media-object
        =/  tag  n:g:elem
        ?+  tag  ~&  "MEDIA OBJECT TAG NOT FOUND: {<tag>}"  accum
        :: TODO: add more tags !!
        :: FIXME: duration=(unit @dr) property of media-object not added. perhaps remove from the media-object data structure ??
          [%media %community]  =.  community.accum  (p-media-community elem)  accum
          [%media %content]  =.  content.accum  (p-media-content elem content.accum)  accum
          [%media %credit]  =.  credits.accum  [(p-media-credit elem) credits.accum]  accum
          [%media %description]  =.  description.accum  (p-text elem)  accum
          [%media %thumbnail]  =.  thumbnails.accum  [(p-media-thumbnail elem) thumbnails.accum]  accum
          [%media %text]  =.  texts.accum  [(p-media-text elem) texts.accum]  accum
          [%media %title]  =.  title.accum  (p-text elem)  accum
        ==
      mol
      |%
      ++  p-media-content
        |=  [el=manx mcl=(list media-content)]
        ^-  (list media-content)
        =/  attrs  a:g:el
        :-
        %+  roll  attrs
          :: check ++p-link for info about `[@tas @tas]`
          |=  [[tag=?(@tas [@tas @tas]) txt=tape] accum=media-content]
          ^-  media-content
          =.  tag  (norm-tag tag ~[['fileSize' 'file-size']] ~)
          ?+  tag  ~&  "MEDIA CONTENT ATTR NOT FOUND: {<tag>}"  accum
            %duration  =.  duration.accum  [~ `@dr`(yule `tarp`[0 0 0 (scan txt dem) ~])]  accum
            %file-size  =.  size.accum  [~ (scan txt dem)]  accum
            %height  =.  height.accum  [~ (scan txt dem)]  accum
            %type  =.  content-type.accum  [~ (crip txt)]  accum
            %url  =.  url.accum  [~ (crip txt)]  accum
            %width  =.  width.accum  [~ (scan txt dem)]  accum
          ==
        mcl
      ++  p-media-credit
        |=  el=manx
        ^-  media-credit
        =/  attrs  a:g:el
        =/  out  %+  roll  attrs
          :: check ++p-link for info about `[@tas @tas]`
          |=  [[tag=?(@tas [@tas @tas]) val=tape] accum=media-credit]
          ^-  media-credit
          =/  txt  [~ (crip val)]
          ?+  tag  !!  :: should never crash
            %role  =.  role.accum  txt  accum
            %scheme  =.  scheme.accum  txt  accum
          ==
        =.  content.out  (need (inner-txt el))
        out
      ++  p-media-thumbnail
        |=  el=manx
        ^-  media-thumbnail
        =/  attrs  a:g:el
        %+  roll  attrs
          :: check ++p-link for info about `[@tas @tas]`
          |=  [[tag=?(@tas [@tas @tas]) txt=tape] accum=media-thumbnail]
          ^-  media-thumbnail
          ?+  tag  ~&  "MEDIA THUMBNAIL ATTR NOT FOUND: {<tag>}"  accum
            %url  =.  uri.image.accum  (crip txt)  accum
            %height  =.  height.image.accum  [~ (scan txt dem)]  accum
            %width  =.  width.image.accum  [~ (scan txt dem)]  accum
          ==
      ++  p-media-community
        |=  el=manx
        ^-  (unit media-community)
        =/  elems  c:el
        :-  ~
        %+  roll  elems
          |=  [elem=manx accum=media-community]
          ^-  media-community
          =/  tag  n:g:elem
          =.  tag  (norm-tag tag ~ ~[[['media' 'starRating'] ['media' 'star-rating']]])
          ?+  tag  ~&  "MEDIA COMMUNITY TAG NOT FOUND: {<tag>}"  accum
          :: TODO: add more tags !!
            [%media %star-rating]  =.  accum  (p-media-stars elem accum)  accum
            [%media %statistics]  =.  accum  (p-media-statistics elem accum)  accum
          ==
      ++  p-media-statistics
        |=  [el=manx mtl=media-community]
        ^-  media-community
        =/  attrs  a:g:el
        =/  mtl-stats  %+  roll  attrs
          :: check ++p-link for info about `[@tas @tas]`
          |=  [[tag=?(@tas [@tas @tas]) txt=tape] accum=media-community]
          ^-  media-community
          ?+  tag  ~&  "MEDIA STATS ATTR NOT FOUND: {<tag>}"  accum
            %views  =.  stats-views.accum  [~ (scan txt dem)]  accum
            %favorites  =.  stats-favorites.accum  [~ (scan txt dem)]  accum
          ==
        :: add media-community stats parsed above to input media-community
        =.  stats-views.mtl  stats-views.mtl-stats
        =.  stats-favorites.mtl  stats-favorites.mtl-stats
        mtl
      ++  p-media-stars
        |=  [el=manx mtl=media-community]
        ^-  media-community
        =/  attrs  a:g:el
        =/  mtl-stars  %+  roll  attrs
          :: check ++p-link for info about `[@tas @tas]`
          |=  [[tag=?(@tas [@tas @tas]) txt=tape] accum=media-community]
          ^-  media-community
          ?+  tag  ~&  "MEDIA STARS ATTR NOT FOUND: {<tag>}"  accum
            %average  =/  frac-tp  ['.' txt]  :: e.g. "5.00" to ".5.00"
              =.  stars-avg.accum  `(unit @rs)`(slaw %rs (crip frac-tp))  accum
            %count  =.  stars-count.accum  [~ (scan txt dem)]  accum
            %min  =.  stars-min.accum  [~ (scan txt dem)]  accum
            %max  =.  stars-max.accum  [~ (scan txt dem)]  accum
          ==
        :: add media-community stars parsed above to input media-community
        =.  stars-avg.mtl  stars-avg.mtl-stars
        =.  stars-count.mtl  stars-count.mtl-stars
        =.  stars-min.mtl  stars-min.mtl-stars
        =.  stars-max.mtl  stars-max.mtl-stars
        mtl
      ++  p-media-text
        |=  el=manx
        ^-  media-text
        =/  attrs  a:g:el
        =/  out  %+  roll  attrs
          :: check ++p-link for info about `[@tas @tas]`
          |=  [[tag=?(@tas [@tas @tas]) val=tape] accum=media-text]
          ^-  media-text
          =/  txt  (crip val)
          ?+  tag  ~&  "MEDIA TEXT ATTR NOT FOUND: {<tag>}"  accum
            :: e.g. "00:00:17.000"
            :: ignoring the ms bc kind useless
            %end  =,  p-date
              =/  hr  `@dr`(yule `tarp`[0 (num (cut-2 txt 0)) 0 0 ~])
              =/  mn  `@dr`(yule `tarp`[0 0 (num (cut-2 txt 3)) 0 ~])
              =/  sc  `@dr`(yule `tarp`[0 0 0 (num (cut-2 txt 6)) ~])
              =.  end-time.accum  [~ `@dr`:(add hr mn sc)]  accum
            %start  =,  p-date
              =/  hr  `@dr`(yule `tarp`[0 (num (cut-2 txt 0)) 0 0 ~])
              =/  mn  `@dr`(yule `tarp`[0 0 (num (cut-2 txt 3)) 0 ~])
              =/  sc  `@dr`(yule `tarp`[0 0 0 (num (cut-2 txt 6)) ~])
              =.  start-time.accum  [~ `@dr`:(add hr mn sc)]  accum
            %type  =.  content-type.text.accum  txt  accum
          ==
        =.  content.text.out  (need (inner-txt el))
        out
      --
    :: normalize tag to correct `term` syntax so able to parse tag elem content
    :: e.g. [%media %starRating] to [%media %star-rating] bc terms can't use capitals
    ++  norm-tag
      |=  [tag=?(@tas [@tas @tas]) a-pairs=(list [@tas @tas]) c-pairs=(list [[@tas @tas] [@tas @tas]])]
      ^-  ?(@tas [@tas @tas])
      ?@  tag
        :: if tag is atom
        =/  crd-tag  ^-  @t  tag
        ^-  @tas
        |-
        :: if list empty / end of list, return original tag
        ?~  a-pairs
          crd-tag
        =/  pair  i:a-pairs
        ?:  =(crd-tag -:pair)
          +:pair
        $(a-pairs t:a-pairs)
      :: if tag is cell
      =/  crd-tag  ^-  [@t @t]  tag
      ^-  [@tas @tas]
      |-
      :: if list empty / end of list, return original tag
      ?~  c-pairs
        crd-tag
      =/  pair  i:c-pairs
      ?:  =(crd-tag -:pair)
        +:pair
      $(c-pairs t:c-pairs)
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
