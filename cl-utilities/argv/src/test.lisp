(in-package :command-line-arguments)

(defparameter *opt-spec*
 '((("all" #\a) :type boolean :documentation "do it all")
   ("blah" :type string :initial-value "blob" :documentation "This is a very long multi line documentation. The function SHOW-OPTION-HELP should display this properly indented, that is all lines should start at the same column.")
   (("verbose" #\v) :type boolean :documentation "include debugging output")
   (("file" #\f) :type string :documentation "read from file instead of standard input")
   (("xml-port" #\x) :type integer :optional t :documentation "specify port for an XML listener")
   (("http-port" #\h) :type integer :initial-value 80 :documentation "specify port for an HTTP listener")
   ("enable-cache" :type boolean :documentation "enable cache for queries")
   ("path" :type string :list t :optional t :documentation "add given directory to the path")
   ("port" :type integer :list (:initial-contents (1 2)) :optional t :documentation "add a normal listen on given port")))

(defun foo (args &key all verbose file xml-port enable-cache port path http-port blah)
  (list args :all all :verbose verbose :file file :xml-port xml-port :http-port http-port :blah blah
        :enable-cache enable-cache :port port :path path))

(multiple-value-bind (options arguments)
    (process-command-line-options
     *opt-spec*
     '("--all" "--no-verbose" "--file" "foo" "-f" "-v" "-v"
       "-x" "--disable-cache" "-h" "8080"
       "--no-port" "--port" "3" "--port=4"
       "--path" "/foo" "--path" "/bar"
       "--" "--foo" "bar" "baz"))
  (write arguments :pretty nil) (terpri)
  (write options :pretty nil) (terpri)
  (write (apply 'foo arguments options) :pretty nil)
  (terpri))

(show-option-help *opt-spec*)
