(in-package #:space-clog-war)

(defparameter *width* 1024)
(defparameter *height* 1024)

(defparameter *debug* nil)

(defun on-key-down (obj event)
  (let ((ot1 (connection-data-item obj "ship-ot1"))
        (ot2 (connection-data-item obj "ship-ot2")))
    (spacewar:update ot1 event)
    (spacewar:update ot2 event)))

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
         (display (display:make-display gl))
         (planetarium (make-instance 'expensive-planetarium:planetarium))
         (label (and *debug* (create-p div-canvas
                                       :content (format nil "~d" (spacewar:x planetarium)))))
         (star (make-instance 'spacewar:star))
         (ship-ot1 (make-instance 'spacewar:ot1 :x 256.0 :y 256.0))
         (ship-ot2 (make-instance 'spacewar:ot2 :x -256.0 :y -256.0 :theta pi))
         (objects (list planetarium ship-ot1 ship-ot2)))
    (setf (connection-data-item body "ship-ot1") ship-ot1
          (connection-data-item body "ship-ot2") ship-ot2)
    (when *debug*
      (set-on-click pause-button (lambda (obj)
                                   (declare (ignore obj))
                                   (setf pausep (if pausep nil t)))))
    (create-gui-menu-item actions :content "Restart" :on-click (lambda (obj) obj))
    (create-gui-menu-item actions :content "Reset" :on-click (lambda (obj) obj))
    (format *debug-io* "~D x ~D~%" (drawing-buffer-width gl) (drawing-buffer-height gl))
    (add-style sb :element "canvas" '(("width" "1024px") ("height" "1024px")))
    (add-class canvas "w3-black")
    (set-border canvas :medium :solid "#0066aa")
    (set-margin canvas "6px" "6px" "6px" "6px")
    (setf (display div-canvas) :flex
          (align-items div-canvas) :center
          (justify-content div-canvas) :center)
    (enable-capability gl :BLEND)
    (blend-function gl :ONE :ONE_MINUS_SRC_ALPHA)
    (clear-color gl 0.0f0 0.0f0 0.0f0 1.0f0)
    (clear-webgl gl :COLOR_BUFFER_BIT)
    (clear-color gl 0.0f0 0.0f0 0.0f0 0.075f0)
    (set-on-key-down body 'on-key-down :disable-default t)
    (loop
      if (or (not *debug*) (not pausep)) do
        (display:clear display)
        (spacewar:draw star display)
        (loop for obj in objects do (spacewar:draw obj display))
        (loop for obj in objects do (spacewar:update obj))
        (display:draw display)
      if *debug* do
        (setf (text label) (format nil "~d" (spacewar:x planetarium)))
      do (sleep 1/60))))

(defun start ()
  "Start Space clog war!."
  (initialize 'on-new-window)
  (open-browser))
