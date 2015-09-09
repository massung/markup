;;;; Markup Encoding/Decoding for Common Lisp
;;;;
;;;; Copyright (c) Jeffrey Massung
;;;;
;;;; This file is provided to you under the Apache License,
;;;; Version 2.0 (the "License"); you may not use this file
;;;; except in compliance with the License.  You may obtain
;;;; a copy of the License at
;;;;
;;;;    http://www.apache.org/licenses/LICENSE-2.0
;;;;
;;;; Unless required by applicable law or agreed to in writing,
;;;; software distributed under the License is distributed on an
;;;; "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
;;;; KIND, either express or implied.  See the License for the
;;;; specific language governing permissions and limitations
;;;; under the License.
;;;;

(defpackage :markup
  (:use :cl :parse :re :lexer)
  (:export

   ;; parsing predicates
   #:*markup-entity-char-p*
   #:*markup-entity-start-char-p*
   #:*markup-entity-level*

   ;; encoding and decoding
   #:markup-encode
   #:markup-decode))

(in-package :markup)

;;; ----------------------------------------------------

(defparameter *markup-entity-start-char-p* 'alpha-char-p
  "Predicate for start character of an entity reference.")

;;; ----------------------------------------------------

(defparameter *markup-entity-char-p* 'alpha-char-p
  "Predicate for subsequent characters of an entity reference.")

;;; ----------------------------------------------------

(defparameter *markup-entity-level* 3
  "The maximum depth to which entities will be expanded.")

;;; ----------------------------------------------------

(defun entity-start-char-p (c)
  "T if the character is a valid start character in an entity reference."
  (funcall *markup-entity-start-char-p* c))

;;; ----------------------------------------------------

(defun entity-char-p (c)
  "T if the character is a valid character in an entity reference."
  (funcall *markup-entity-char-p* c))

;;; ----------------------------------------------------

(defun entity-ref (name)
  "Returns the character for a given entity name."
  (let ((common-entities '(("lt"       . #\<)
                           ("gt"       . #\>)
                           ("amp"      . #\&)
                           ("quot"     . #\")
                           ("apos"     . #\')
                           ("nbsp"     . #\space)

                           ;; common, but non-standard
                           ("iexcl"    . #\u+00a1)
                           ("cent"     . #\u+00a2)
                           ("pound"    . #\u+00a3)
                           ("curren"   . #\u+00a4)
                           ("yen"      . #\u+00a5)
                           ("brvbar"   . #\u+00a6)
                           ("sect"     . #\u+00a7)
                           ("uml"      . #\u+00a8)
                           ("copy"     . #\u+00a9)
                           ("ordf"     . #\u+00aa)
                           ("laquo"    . #\u+00ab)
                           ("not"      . #\u+00ac)
                           ("shy"      . #\u+00ad)
                           ("reg"      . #\u+00ae)
                           ("macr"     . #\u+00af)
                           ("deg"      . #\u+00b0)
                           ("plusmn"   . #\u+00b1)
                           ("sup2"     . #\u+00b2)
                           ("sup3"     . #\u+00b3)
                           ("acute"    . #\u+00b4)
                           ("micro"    . #\u+00b5)
                           ("para"     . #\u+00b6)
                           ("middot"   . #\u+00b7)
                           ("cedil"    . #\u+00b8)
                           ("sup1"     . #\u+00b9)
                           ("ordm"     . #\u+00ba)
                           ("raquo"    . #\u+00bb)
                           ("frac14"   . #\u+00bc)
                           ("frac12"   . #\u+00bd)
                           ("frac34"   . #\u+00be)
                           ("iquest"   . #\u+00bf)
                           ("Agrave"   . #\u+00c0)
                           ("Aacute"   . #\u+00c1)
                           ("Acirc"    . #\u+00c2)
                           ("Atilde"   . #\u+00c3)
                           ("Auml"     . #\u+00c4)
                           ("Aring"    . #\u+00c5)
                           ("AElig"    . #\u+00c6)
                           ("Ccedil"   . #\u+00c7)
                           ("Egrave"   . #\u+00c8)
                           ("Eacute"   . #\u+00c9)
                           ("Ecirc"    . #\u+00ca)
                           ("Euml"     . #\u+00cb)
                           ("Igrave"   . #\u+00cc)
                           ("Iacute"   . #\u+00cd)
                           ("Icirc"    . #\u+00ce)
                           ("Iuml"     . #\u+00cf)
                           ("ETH"      . #\u+00d0)
                           ("Ntilde"   . #\u+00d1)
                           ("Ograve"   . #\u+00d2)
                           ("Oacute"   . #\u+00d3)
                           ("Ocirc"    . #\u+00d4)
                           ("Otilde"   . #\u+00d5)
                           ("Ouml"     . #\u+00d6)
                           ("times"    . #\u+00d7)
                           ("Oslash"   . #\u+00d8)
                           ("Ugrave"   . #\u+00d9)
                           ("Uacute"   . #\u+00da)
                           ("Ucirc"    . #\u+00db)
                           ("Uuml"     . #\u+00dc)
                           ("Yacute"   . #\u+00dd)
                           ("THORN"    . #\u+00de)
                           ("szlig"    . #\u+00df)
                           ("agrave"   . #\u+00e0)
                           ("aacute"   . #\u+00e1)
                           ("acirc"    . #\u+00e2)
                           ("atilde"   . #\u+00e3)
                           ("auml"     . #\u+00e4)
                           ("aring"    . #\u+00e5)
                           ("aelig"    . #\u+00e6)
                           ("ccedil"   . #\u+00e7)
                           ("egrave"   . #\u+00e8)
                           ("eacute"   . #\u+00e9)
                           ("ecirc"    . #\u+00ea)
                           ("euml"     . #\u+00eb)
                           ("igrave"   . #\u+00ec)
                           ("iacute"   . #\u+00ed)
                           ("icirc"    . #\u+00ee)
                           ("iuml"     . #\u+00ef)
                           ("eth"      . #\u+00f0)
                           ("ntilde"   . #\u+00f1)
                           ("ograve"   . #\u+00f2)
                           ("oacute"   . #\u+00f3)
                           ("ocirc"    . #\u+00f4)
                           ("otilde"   . #\u+00f5)
                           ("ouml"     . #\u+00f6)
                           ("divide"   . #\u+00f7)
                           ("oslash"   . #\u+00f8)
                           ("ugrave"   . #\u+00f9)
                           ("uacute"   . #\u+00fa)
                           ("ucirc"    . #\u+00fb)
                           ("uuml"     . #\u+00fc)
                           ("yacute"   . #\u+00fd)
                           ("thorn"    . #\u+00fe)
                           ("yuml"     . #\u+00ff)
                           ("OElig"    . #\u+0152)
                           ("oelig"    . #\u+0153)
                           ("Scaron"   . #\u+0160)
                           ("scaron"   . #\u+0161)
                           ("Yuml"     . #\u+0178)
                           ("fnof"     . #\u+0192)
                           ("circ"     . #\u+02c6)
                           ("tilde"    . #\u+02dc)
                           ("Alpha"    . #\u+0391)
                           ("Beta"     . #\u+0392)
                           ("Gamma"    . #\u+0393)
                           ("Delta"    . #\u+0394)
                           ("Epsilon"  . #\u+0395)
                           ("Zeta"     . #\u+0396)
                           ("Eta"      . #\u+0397)
                           ("Theta"    . #\u+0398)
                           ("Iota"     . #\u+0399)
                           ("Kappa"    . #\u+039a)
                           ("Lambda"   . #\u+039b)
                           ("Mu"       . #\u+039c)
                           ("Nu"       . #\u+039d)
                           ("Xi"       . #\u+039e)
                           ("Omicron"  . #\u+039f)
                           ("Pi"       . #\u+03a0)
                           ("Rho"      . #\u+03a1)
                           ("Sigma"    . #\u+03a3)
                           ("Tau"      . #\u+03a4)
                           ("Upsilon"  . #\u+03a5)
                           ("Phi"      . #\u+03a6)
                           ("Chi"      . #\u+03a7)
                           ("Psi"      . #\u+03a8)
                           ("Omega"    . #\u+03a9)
                           ("alpha"    . #\u+03b1)
                           ("beta"     . #\u+03b2)
                           ("gamma"    . #\u+03b3)
                           ("delta"    . #\u+03b4)
                           ("epsilon"  . #\u+03b5)
                           ("zeta"     . #\u+03b6)
                           ("eta"      . #\u+03b7)
                           ("theta"    . #\u+03b8)
                           ("iota"     . #\u+03b9)
                           ("kappa"    . #\u+03ba)
                           ("lambda"   . #\u+03bb)
                           ("mu"       . #\u+03bc)
                           ("nu"       . #\u+03bd)
                           ("xi"       . #\u+03be)
                           ("omicron"  . #\u+03bf)
                           ("pi"       . #\u+03c0)
                           ("rho"      . #\u+03c1)
                           ("sigmaf"   . #\u+03c2)
                           ("sigma"    . #\u+03c3)
                           ("tau"      . #\u+03c4)
                           ("upsilon"  . #\u+03c5)
                           ("phi"      . #\u+03c6)
                           ("chi"      . #\u+03c7)
                           ("psi"      . #\u+03c8)
                           ("omega"    . #\u+03c9)
                           ("thetasym" . #\u+03d1)
                           ("upsih"    . #\u+03d2)
                           ("piv"      . #\u+03d6)
                           ("ensp"     . #\u+2002)
                           ("emsp"     . #\u+2003)
                           ("thinsp"   . #\u+2009)
                           ("zwnj"     . #\u+200c)
                           ("zwj"      . #\u+200d)
                           ("lrm"      . #\u+200e)
                           ("rlm"      . #\u+200f)
                           ("ndash"    . #\u+2013)
                           ("mdash"    . #\u+2014)
                           ("lsquo"    . #\u+2018)
                           ("rsquo"    . #\u+2019)
                           ("sbquo"    . #\u+201a)
                           ("ldquo"    . #\u+201c)
                           ("rdquo"    . #\u+201d)
                           ("bdquo"    . #\u+201e)
                           ("dagger"   . #\u+2020)
                           ("Dagger"   . #\u+2021)
                           ("bull"     . #\u+2022)
                           ("hellip"   . #\u+2026)
                           ("permil"   . #\u+2030)
                           ("prime"    . #\u+2032)
                           ("Prime"    . #\u+2033)
                           ("lsaquo"   . #\u+2039)
                           ("rsaquo"   . #\u+203a)
                           ("oline"    . #\u+203e)
                           ("frasl"    . #\u+2044)
                           ("euro"     . #\u+20ac)
                           ("image"    . #\u+2111)
                           ("weierp"   . #\u+2118)
                           ("real"     . #\u+211c)
                           ("trade"    . #\u+2122)
                           ("alefsym"  . #\u+2135)
                           ("larr"     . #\u+2190)
                           ("uarr"     . #\u+2191)
                           ("rarr"     . #\u+2192)
                           ("darr"     . #\u+2193)
                           ("harr"     . #\u+2194)
                           ("crarr"    . #\u+21b5)
                           ("lArr"     . #\u+21d0)
                           ("uArr"     . #\u+21d1)
                           ("rArr"     . #\u+21d2)
                           ("dArr"     . #\u+21d3)
                           ("hArr"     . #\u+21d4)
                           ("forall"   . #\u+2200)
                           ("part"     . #\u+2202)
                           ("exist"    . #\u+2203)
                           ("empty"    . #\u+2205)
                           ("nabla"    . #\u+2207)
                           ("isin"     . #\u+2208)
                           ("notin"    . #\u+2209)
                           ("ni"       . #\u+220b)
                           ("prod"     . #\u+220f)
                           ("sum"      . #\u+2211)
                           ("minus"    . #\u+2212)
                           ("lowast"   . #\u+2217)
                           ("radic"    . #\u+221a)
                           ("prop"     . #\u+221d)
                           ("infin"    . #\u+221e)
                           ("ang"      . #\u+2220)
                           ("and"      . #\u+2227)
                           ("or"       . #\u+2228)
                           ("cap"      . #\u+2229)
                           ("cup"      . #\u+222a)
                           ("int"      . #\u+222b)
                           ("there4"   . #\u+2234)
                           ("sim"      . #\u+223c)
                           ("cong"     . #\u+2245)
                           ("asymp"    . #\u+2248)
                           ("ne"       . #\u+2260)
                           ("equiv"    . #\u+2261)
                           ("le"       . #\u+2264)
                           ("ge"       . #\u+2265)
                           ("sub"      . #\u+2282)
                           ("sup"      . #\u+2283)
                           ("nsub"     . #\u+2284)
                           ("sube"     . #\u+2286)
                           ("supe"     . #\u+2287)
                           ("oplus"    . #\u+2295)
                           ("otimes"   . #\u+2297)
                           ("perp"     . #\u+22a5)
                           ("sdot"     . #\u+22c5)
                           ("vellip"   . #\u+22ee)
                           ("lceil"    . #\u+2308)
                           ("rceil"    . #\u+2309)
                           ("lfloor"   . #\u+230a)
                           ("rfloor"   . #\u+230b)
                           ("lang"     . #\u+2329)
                           ("rang"     . #\u+232a)
                           ("loz"      . #\u+25ca)
                           ("spades"   . #\u+2660)
                           ("clubs"    . #\u+2663)
                           ("hearts"   . #\u+2665)
                           ("diams"    . #\u+2666))))

    ;; lookup the entity from the default list
    (cdr (assoc name common-entities :test #'string=))))

;;; ----------------------------------------------------

(define-lexer markup-lexer (s)

  ;; character references
  ("&#[xX](%x+);" (values :char-ref (parse-integer $1 :radix 16)))
  ("&#(%d+);" (values :char-ref (parse-integer $1 :radix 10)))

  ;; entity references
  ("&(%:entity-start-char-p:%:entity-char-p:*);" (values :entity-ref $1))

  ;; anything else (including malformed references)
  (".[^&]*" (values :text $$)))

;;; ----------------------------------------------------

(define-parser markup-parser
  "Parse references from a string and return a list of values."
  (.let (cs (.many (.either (.is :text) 'ref-parser)))
    (.ret (format nil "~{~a~}" cs))))

;;; ----------------------------------------------------

(define-parser ref-parser
  "Parse a character or entity reference."
  (.or (.let (ref (.is :char-ref))
         (.ret (code-char ref)))

       ;; entity references need to recursively expand
       (.let* ((ref (.is :entity-ref))

               ;; get the document entities
               (entity-callback (.get)))

         ;; try and expand the reference
         (let ((value (or (when entity-callback
                            (funcall entity-callback ref))
                          (entity-ref ref))))
           (typecase value

             ;; unknown entity, just return the entity name
             (null      (.ret ref))

             ;; characters just return themselves
             (character (.ret value))

             ;; strings can have markup inside them, recursively decode
             (string    (.ret (markup-decode value entity-callback)))

             ;; everything else should princ to a value
             (otherwise (.ret (princ-to-string value))))))))

;;; ----------------------------------------------------

(defun markup-encode (string)
  "Replace characters from HTML to &entity; references."
  (with-output-to-string (s)
    (flet ((encode (c)
             (case c
               (#\" (write-string "&quot;" s))
               (#\' (write-string "&apos;" s))
               (#\< (write-string "&lt;" s))
               (#\> (write-string "&gt;" s))
               (#\& (write-string "&amp;" s))

               ;; non-standard entity characters
               (otherwise (let ((n (char-code c)))
                            (if (<= 32 n 127)
                                (write-char c s)
                              (format s"&#~4,'0d;" n)))))))

      ;; encode each character and write it to the stream
      (map nil #'encode string))))

;;; ----------------------------------------------------

(defun markup-decode (str &optional entity-callback)
  "Decode a string, replacing &entity; references."
  (if (zerop *markup-entity-level*)
      str
    (let ((*markup-entity-level* (1- *markup-entity-level*)))
      (with-lexer (lexer 'markup-lexer str)
        (with-token-reader (next-token lexer)
          (parse 'markup-parser next-token :initial-state entity-callback))))))
