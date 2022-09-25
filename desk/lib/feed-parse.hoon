:: this lib is based on https://github.com/feed-rs/feed-rs (fave feed lib !!)
:: atom specs: https://validator.w3.org/feed/docs/atom.html
=<
|%
+$  category
  $:  term=@t
      scheme=(unit @t)
      label=(unit @t)
  ==
+$  content
  $:  body=(unit @t)
      :: should be mime, see $+text for more details
      content-type=@t
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
      :: date in UTC timezone
      published=(unit @da)
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
  $:  uri=@t
      title=(unit @t)
      link=(unit link)
      width=(unit @ud)
      height=(unit @ud)
      description=(unit @t)
  ==
+$  link
  $:  href=@t
      rel=(unit @t)
      media-type=(unit @t)
      href-lang=(unit @t)
      title=(unit @t)
      length=(unit @ud)
  ==
+$  media-community
  $:  stars-avg=(unit @rs)
      stars-count=(unit @ud)
      stars-min=(unit @ud)
      stars-max=(unit @ud)
      stats-views=(unit @ud)
      stats-favorites=(unit @ud)
  ==
+$  media-content
  $:  url=(unit purl:eyre)
      :: should be (unit mime), see $+text for more details
      content-type=(unit @t)
      height=(unit @ud)
      width=(unit @ud)
      duration=(unit @dr)
      size=(unit @ud)
      rating=(unit media-rating)
  ==
+$  media-credit  entity=@t
+$  media-object
  $:  title=(unit text)
      content=(list media-content)
      duration=(unit @dr)
      thumbnails=(list media-thumbnail)
      texts=(list media-text)
      description=(unit text)
      community=(unit media-community)
      credits=(list media-credit)
  ==
+$  media-rating
  $:  urn=@t
      value=@t
  ==
+$  media-text
  $:  =text
      start-time=(unit @dr)
      end-time=(unit @dr)
  ==
+$  media-thumbnail
  $:  =image
      time=(unit @dr)
  ==
+$  person
  $:  name=@t
      uri=(unit @t)
      email=(unit @t)
  ==
+$  text
      :: feed-rs uses type `mime` for  `content-type`, but idk how to do enums 
      :: with default vals, so just doing str instead
      :: if figure out, here is the mimes data type to copy:
      :: https://docs.rs/mime/0.3.16/mime/index.html
  $:  content-type=@t
      src=(unit @t)
      content=@t
  ==
::
++  parse-feed
  =<
  |=  feed-str=@t
  ^-  feed
  :: get first char of feed `cord` to determine is json or xml
  ?:  =(`@t`(cut 3 [0 1] feed-str) '{')
    (json-parse feed-str)
  :: (xml-parse feed-str)
  *feed
  |%
  ++  json-parse
    =<
    |=  feed-str=@t
    ^-  feed
    =/  j  (need (de-json:html feed-str))  :: if crash, good :)
    =*  inp  (get-obj j)
    =|  out=feed
    :: ~&  inp
    :: =.  authors.out  (p-authors inp)
    =.  title.out  [~ content-type='text' src=~ content=(get-str-attr inp 'title')]
    ::
    ~&  "TEST"
    ::
    ~&  (p-entries inp)
    ::
    :: =/  obj-val  [[p='version' q=[%o p=[p='version' q=[%s p='https://jsonfeed.org/version/1']]]]]
    :: ~&  (get-obj-attr obj-val 'version')
    ::
    ::
    !!
    :: --parsing the diff types--
    =<
    |%
    ++  p-authors
      |*  obj-val=(map @t *)
      =/  arr  (get-arr-attr obj-val 'authors')
      !!
    :: convert @t of json feed to @da
      ++  p-entries
      =<
      |=  obj-val=(map @t json)
      =/  arr  (get-arr-attr obj-val 'items')
      =|  entry=entry
      =/  i  1
      :: |-
      =/  o  (get-obj (do-the-thing arr i))
      :: published=(unit @da)
      ~&  (get-str-attr o 'date_published')
      ~&  (p-date (get-str-attr o 'date_published'))
      =.  published.entry  [~ (p-date (get-str-attr o 'date_published'))]
      ~&  (need published.entry)
      :: $(i +(i))
      !!
      |%
      ++  do-the-thing  :: TODO: maybe should conv to normie manual list loop 🤔
        |=  [arr=(list json) i=@ud]
        ?+  i  !!
          %1  &1:arr
          %2  &2:arr
          %3  &3:arr
          %4  &4:arr
          %5  &5:arr
          %6  &6:arr
          %7  &7:arr
          %8  &8:arr
          %9  &9:arr
          %10  &10:arr
          %11  &11:arr
          %12  &12:arr
          %13  &13:arr
          %14  &14:arr
          %15  &15:arr
          %16  &16:arr
          %17  &17:arr
          %18  &18:arr
          %19  &19:arr
          %20  &20:arr
        ==
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
      |=  [obj-val=(map @t json) key=@t]
      ^-  (list json)
      =/  attr  (need (~(get by obj-val) key))
      ?>  ?=([%a p=*] attr)  :: for some reason i need to add `p=` to be able 
      p:attr                 :: to find p for `%s` and `%a`, but not for `%o`
    :: get string value using key on object
    ++  get-str-attr
      |=  [obj-val=(map @t json) key=@t]
      ^-  @t
      =/  attr  (need (~(get by obj-val) key))
      ?>  ?=([%s p=@t] attr)  :: for some reason i need to add `p=` to be able 
      p:attr                  :: to find p for `%s`, but not for `%o`
    --
  ::
  ++  xml-parse
    =<
    |=  feed-str=@t
    ^-  feed
    =/  xml  (need (de-xml:html feed-str))
    ~&  n:g:xml
    =*  elem-list  c:xml
    ?:  =(n:g:xml %feed)  :: if this is %feed, it's an atom feed, else it's a rss feed
      !!
    :: loop through each xml base-attribute, with a switch statment procossing 
    :: each element. including other arms that e.g. does the processing 
    :: (looping through elements/attributes) for an author, etc
    |-
    !!
    =<
    |%  :: --rss parsing--
    --
    |%  :: --atom parsing--
    --
  --
--
:: --general helper functions--
|%
++  p-date
  =<
  |=  t=@t
  ^-  @da
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
  ^-  @da  ^-  @
  ~&  "crash here?"
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
