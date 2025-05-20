(in-package #:space-clog-war)

(defparameter *width* 1024)
(defparameter *height* 1024)
(defparameter *time* 1/60)
(defparameter *debug* nil)

(defun on-key-down (obj event)
  (let ((ot1 (connection-data-item obj "ship-ot1"))
        (ot2 (connection-data-item obj "ship-ot2")))
    (string-case:string-case ((getf event :key) :default nil)
      ("ArrowRight" (setf (spacewar:right ot1) t))
      ("ArrowLeft" (setf (spacewar:left ot1) t))
      ("ArrowDown" (setf (spacewar:down ot1) t))
      ("ArrowUp" (setf (spacewar:up ot1) t))
      ("d" (setf (spacewar:right ot2) t))
      ("a" (setf (spacewar:left ot2) t))
      ("s" (setf (spacewar:down ot2) t))
      ("w" (setf (spacewar:up ot2) t)))))

(defun on-key-up (obj event)
  (let ((ot1 (connection-data-item obj "ship-ot1"))
        (ot2 (connection-data-item obj "ship-ot2")))
    (string-case:string-case ((getf event :key) :default nil)
      ("ArrowRight" (setf (spacewar:right ot1) nil))
      ("ArrowLeft" (setf (spacewar:left ot1) nil))
      ("ArrowDown" (setf (spacewar:down ot1) nil))
      ("ArrowUp" (setf (spacewar:up ot1) nil))
      ("d" (setf (spacewar:right ot2) nil))
      ("a" (setf (spacewar:left ot2) nil))
      ("s" (setf (spacewar:down ot2) nil))
      ("w" (setf (spacewar:up ot2) nil)))))

(defun on-new-window (body)
  (when *debug*
    (debug-mode body))
  (setf (title (html-document body)) "Space clog war!")
  (load-css (html-document body)
            "https://cdn.jsdelivr.net/npm/bootstrap@5.3.3/dist/css/bootstrap.min.css")
  (load-script (html-document body)
               "https://cdn.jsdelivr.net/npm/bootstrap@5.3.3/dist/js/bootstrap.bundle.min.js")
  (clog-gui-initialize body)
  (add-class body "w3-blue")
  (let* ((actions (create-gui-menu-drop-down (create-gui-menu-bar body)
                                             :content "Space clog war!"))
         (container (create-div body :class "container-fluid"))
         (div-canvas (create-div (create-div container :class "row") :class "col"))
         (canvas (create-canvas div-canvas :class "img-fluid" :width *width*
                                           :height *height*))
         (gl (create-webgl canvas :attributes '("preserveDrawingBuffer" t
                                                "powerPreference" "low-power"
                                                "antialias" t)))
         (sb (create-style-block body))
         (pausep nil)
         (pause-button (and *debug* (create-button div-canvas :content "Pause"
                                                              :class "btn btn-primary")))
         (planetarium (make-instance 'expensive-planetarium:planetarium))
         (label (and *debug* (create-p div-canvas
                                       :content (format nil "~d" (spacewar:x planetarium)))))
         (star (make-instance 'spacewar:star))
         (ship-ot1 (make-instance 'spacewar:ot1 :x 256.0 :y 256.0
                                                :theta 0.0 :dx 0.0 :dy 0.0))
         (ship-ot2 (make-instance 'spacewar:ot2 :x -256.0 :y -256.0
                                                :theta pi :dx 0.0 :dy 0.0))
         (context (make-instance 'spacewar:context
                                 :display (display:make-display gl)
                                 :objects (list planetarium ship-ot1 ship-ot2))))
    (setf (connection-data-item body "ship-ot1") ship-ot1
          (connection-data-item body "ship-ot2") ship-ot2)
    (when *debug*
      (set-on-click pause-button (lambda (obj)
                                   (declare (ignore obj))
                                   (setf pausep (if pausep nil t)))))
    (create-gui-menu-item actions :content "Restart" :on-click (lambda (obj) obj))
    (create-gui-menu-item actions :content "Reset" :on-click (lambda (obj) obj))
    (format *debug-io* "~D x ~D~%" (drawing-buffer-width gl) (drawing-buffer-height gl))
    (add-style sb :element "canvas" '(("width" "90vmin") ("height" "90vmin")))
    (add-class canvas "w3-black")
    (set-border canvas :medium :solid "#0066aa")
    (set-margin canvas "6px" "6px" "6px" "6px")
    (setf (display div-canvas) :flex
          (align-items div-canvas) :center
          (justify-content div-canvas) :center)
    (enable-capability gl :BLEND)
    ;; (blend-equation-seperate gl :FUNC_ADD :FUNC_ADD)
    (blend-function gl :SRC_COLOR :CONSTANT_COLOR)
    (clear-color gl 0.0f0 0.0f0 0.0f0 1.000f0)
    (blend-color gl 0.91f0 0.91f0 0.91f0 0.95f0)
    (clear-webgl gl :COLOR_BUFFER_BIT)
    (set-on-key-down body 'on-key-down :disable-default t)
    (set-on-key-up body 'on-key-up)
    (loop
      if (or (not (validp body)) (connection-data-item body "done"))
        return (values)
      if (or (not *debug*) (not pausep)) do
        (display:clear (spacewar:display context))
        (spacewar:draw star (spacewar:display context))
        (loop for obj in (spacewar:objects context) do (spacewar:draw obj (spacewar:display context)))
        (loop for obj in (spacewar:objects context)
              do (spacewar:update obj context)
              if (spacewar:new-objects obj)
                do (loop for new = (pop (spacewar:new-objects obj))
                         while new
                         do (push new (spacewar:objects context))))
        (display:draw (spacewar:display context))
      if *debug* do
        (setf (text label) (format nil "~d" (spacewar:x planetarium)))
      do (sleep *time*))))

(defun start ()
  "Start Space clog war!."
  (initialize 'on-new-window)
  (open-browser))
