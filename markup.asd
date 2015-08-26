(defpackage :markup-asd
  (:use :cl :asdf))

(in-package :markup-asd)

(defsystem :markup
  :name "markup"
  :version "1.0"
  :author "Jeffrey Massung"
  :license "Apache 2.0"
  :description "Markup encoding/decoding for Common Lisp."
  :serial t
  :components ((:file "markup"))
  :depends-on ("parse" "lexer" "re"))
