(in-package :bloom)

(defun ensure-prefab-name-string (name)
  (unless (stringp name)
    (error "Prefab name ~s must be a string." name)))

(defun ensure-prefab-name-valid (name)
  (when (find #\/ name)
    (error "Prefab name ~s must not contain a \"/\" character." name)))

(defun ensure-path-string (path)
  (unless (stringp path)
    (error "Path must be a string: ~s." path)))

(defun ensure-path-length (parent path)
  (when (zerop (length path))
    (error "Cannot have an empty child path.~%Path: ~s." parent)))

(defun ensure-path-relative (path)
  (when (au:string-starts-with-p path "/")
    (error "Path must be a relative path: ~s." path)))

(defun ensure-path-no-trailing-slash (path)
  (when (au:string-ends-with-p path "/")
    (error "Path must not have a trailing \"/\" character.~%Path: ~s." path)))

(defun ensure-path-valid (path)
  (loop :for x = #\Nul :then y
        :for y :across path
        :when (char= x y #\/)
          :do (error "A path cannot contain adjacent \"/\" characters.~%~
                      Path: ~s."
                     path)))

(defun ensure-path-options-plist (path options)
  (unless (au:plist-p options)
    (error "Path options must be a property list of keyword keys and values.~%~
            Path: ~s."
           path)))

(defun ensure-path-options-valid (path options)
  (let ((valid-options '(:copy :link)))
    (dolist (x (au:plist-keys options))
      (unless (member x valid-options)
        (error "Invalid path option: ~s.~%Valid options: ~{~s~^, ~}Path: ~s."
               x valid-options path)))))

(defun ensure-path-options-keys (path options)
  (destructuring-bind (&key copy link) options
    (when (and copy link)
      (error "Only one of :COPY or :LINK can be specified for a path.~%Path: ~s."
             path))))

(defun ensure-path-single-transform (components path)
  (when (> (count 'transform components :key #'car) 1)
    (error "Cannot have multiple transform components per path.~%Path: ~s."
           path)))

(defun ensure-copy/link-source-string (path source)
  (unless (stringp source)
    (error "The source of a :LINK or :COPY must be a string.~%Path: ~s." path)))

(defun ensure-copy/link-source-absolute (path source)
  (unless (au:string-starts-with-p source "/")
    (error "The source of a :LINK or :COPY must be an absolute path.~%Path: ~s."
           path)))

(defun ensure-copy/link-source-no-trailing-slash (path source)
  (when (au:string-ends-with-p source "/")
    (error "The source of a :LINK or :COPY must not have a trailing \"/\" ~
            character.~%Path: ~s."
           path)))

(defun ensure-copy/link-source-valid (path source)
  (loop :for x = #\Nul :then y
        :for y :across source
        :when (char= x y #\/)
          :do (error "The source of a :LINK or :COPY cannot contain adjacent ~
                      \"/\" characters.~%Path: ~s."
                     path)))

(defun ensure-component-list (component path)
  (unless (listp component)
    (error "Component form must be a list: ~s.~%Path: ~s." component path)))

(defun ensure-component-form (component path)
  (unless (and (> (length component) 1)
               (listp (second component)))
    (error "Component form:~%~s~%must be a list of the form: (TYPE (OPTIONS) ~
            ARGS).~%Path: ~s"
           component path)))

(defun ensure-component-not-duplicate (node type id)
  (with-slots (%path %components-table) node
    (when (au:href %components-table type id)
      (error "Duplicate component type: ~s with the same ID: ~s, and no policy ~
              set.~%Valid policies: NEW-TYPE, OLD-TYPE, NEW-ARGS, OLD-ARGS.~%~
              Path: ~s."
             type id %path))))

(defun ensure-component-type-symbol (type path)
  (unless (and (symbolp type)
               (not (keywordp type)))
    (error "Component type ~s must be a non-keyword symbol.~%Path: ~s"
           type path)))

(defun ensure-component-type-exists (type path)
  (unless (get-computed-component-precedence-list type)
    (error "Component type ~s does not exist.~%Path: ~s." type path)))

(defun ensure-component-options-plist (type options path)
  (unless (au:plist-p options)
    (error "Component options for type ~s must be a property list of keyword ~
            keys and values.~%Path: ~s."
           type path)))

(defun ensure-component-options-valid (type options path)
  (let ((valid-options '(:id :policy)))
    (dolist (x (au:plist-keys options))
      (unless (member x valid-options)
        (error "Component type ~s has an invalid option: ~s.~%Valid options: ~
                ~{~s~^, ~}.~%Path: ~s."
               type x valid-options path)))))

(defun ensure-component-id (type options path)
  (au:when-let ((id (getf options :id)))
    (unless (integerp id)
      (error "Component type ~s must have an integer ID.~%Path: ~s."
             type path))))

(defun ensure-component-policy (type options path)
  (au:when-let ((policy (getf options :policy))
                (valid-policies '(:new-type :old-type :new-args :old-args)))
    (unless (member policy valid-policies)
      (error "Component type ~s has an invalid policy: ~s.~%Valid policies: ~
              ~{~s~^, ~}.~%Path: ~s."
             type policy valid-policies path))))

(defun ensure-component-args-plist (type args path)
  (unless (au:plist-p args)
    (error "Component arguments for type ~s must be an even number of keyword ~
            keys and values.~%Path: ~s."
           type path)))

(defun ensure-component-args-valid (type args path)
  (loop :with valid-args = (compute-component-initargs type)
        :for (key nil) :on args :by #'cddr
        :unless (member key valid-args)
          :do (error "Component type ~s has an invalid argument: ~s.~%Path: ~s."
                     type key path)))
