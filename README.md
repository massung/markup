# Markup encoding/decoding for Common Lisp

There are a lot of markup-esque languages out there (HTML, XML, ...). This is just a simple package that can be used by higher-level markup languages for encoding and decoding strings with tags and character/entity references in them for Common Lisp.

## Quickstart

There are only 2 functions exported:

    (markup-encode string)
    (markup-decode string &key entities)

Encoding with `markup-encode` will properly convert all standardly encoded characters (e.g. &, <, >, ", and ') into their named entity. All other non-ASCII characters will encode into a character reference.

    CL-USER > (markup-encode "<This & That>")
    "&lt;This &amp; That&gt;"

Likewise, `markup-decode` will perform the inverse.

    CL-USER > (markup-decode *)
    "<This & That>"

*Note: encoding and decoding will always return a new string, even if there is nothing to encode or decode.*

The `markup-decode` function also takes an optional *entities* keyword argument. This allows for passing in of document-specific entities. There are already a plethora of common entities that are tested against when decoding, but (XML for example) some markup languages allow the user to define their own entities. If passed in, document entities take priority.

    CL-USER > (markup-decode "&bull; Item 1.a. is &cool;" :entities '(("cool" . "AWESOME!")))
    "• Item 1.a. is AWESOME!"

## Customizing Entity References

Different markup languages use different predicates for what make up a valid entity reference character. You can override the predicates used with the special variables `*markup-entity-start-char-p*` and `*markup-entity-char-p*`.

*Note: by default, these two variables default to `alpha-char-p`.*

Simply set these to what you'd like them to be for your markup language and then decode the markup. For example:

    (let ((*markup-entity-start-char-p* 'xml-name-char-p)
          (*markup-entity-char-p* 'xml-token-char-p))
      (markup-decode "&my-valid.xml:entity;"))

That's it!
