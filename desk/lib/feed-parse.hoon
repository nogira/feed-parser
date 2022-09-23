:: this lib is based on https://github.com/feed-rs/feed-rs (fave feed lib !!)
:: atom specs: https://validator.w3.org/feed/docs/atom.html
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
  (xml-parse feed-str)
  :: *feed
  |%
  ++  json-parse
    =<
    |=  feed-str=@t
    ^-  feed
    =/  j  (need (de-json:html feed-str))  :: if crash, good :)
    =*  inp  (get-obj j)
    =|  out=feed
    ~&  inp
    =.  authors.out  
    =.  title.out  [~ content-type='text' src=~ content=(get-str-attr inp 'title')]
    !!
    :: --manual json parsing helper functions--
    |%
    :: get `*` from `[%o *]`
    ++  get-obj
      |=  obj=json
      ?>  ?=([%o *] obj)
      p:obj
    :: get string value using key on object
    ++  get-str-attr
      |=  [obj-val=(map @t *) key=@t]
      ^-  @t
      =/  attr  (need (~(get by obj-val) key))
      ?>  ?=([%s p=@t] attr)  :: for some reason i need to add `p=` to be able 
      p:attr                  :: to find p for `%s`, but not for `%o`
    --
  ::
  ++  xml-parse
    |=  feed-str=@t
    ^-  feed
    =/  xml  (need (de-xml:html feed-str))
    ~&  n:g:xml  :: if this is %feed, it's an atom feed, else it's a rss feed
    =*  elem-list  c:xml
    :: loop through each xml base-attribute, with a switch statment procossing 
    :: each element. including other arms that e.g. does the processing 
    :: (looping through elements/attributes) for an author, etc
    |-
    !!
  --
--
