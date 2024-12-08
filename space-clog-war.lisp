(in-package #:space-clog-war)

(defparameter *width* 1024)
(defparameter *height* 1024)

(defparameter *debug* nil)

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
  (let* ((actions (create-gui-menu-drop-down (create-gui-menu-bar body) :content "Space clog war!"))
         (container (create-div body :class "container-fluid"))
         (div-canvas (create-div (create-div container :class "row") :class "col"))
         (canvas (create-canvas div-canvas :class "img-fluid" :width *width* :height *height*))
         (gl (create-webgl canvas :attributes '("preserveDrawingBuffer" t
                                                "powerPreference" "low-power"
                                                "antialias" t)))
         (display (display:make-display gl))
         (planetarium (make-instance 'expensive-planetarium:planetarium))
         (star (make-instance 'spacewar:star))
         (sb (create-style-block body))
         (pausep nil)
         (pause-button (and *debug* (create-button div-canvas :content "Pause" :class "btn btn-primary")))
         (label (and *debug* (create-p div-canvas
                                       :content (format nil "~d" (spacewar:x planetarium)))))
         (ship-ot1 (make-instance 'spacewar::ot1 :x 256.0 :y 256.0))
         (ship-ot2 (make-instance 'spacewar::ot2 :x -256.0 :y -256.0 :theta pi)))
    (when *debug*
      (set-on-click pause-button (lambda (obj)
                                   (declare (ignore obj))
                                   (setf pausep (if pausep nil t)))))
    (create-gui-menu-item actions :content "Restart" :on-click (lambda (obj) obj))
    (create-gui-menu-item actions :content "Reset" :on-click (lambda (obj) obj))
    (format *debug-io* "~D x ~D~%" (drawing-buffer-width gl) (drawing-buffer-height gl))
    (add-style sb :element "canvas" '(("width" "600px") ("height" "600px")))
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
    (loop
      if (or (not *debug*) (not pausep)) do
        (display:clear display)
        (spacewar:draw star display)
        (spacewar:draw planetarium display)
        (spacewar:draw ship-ot1 display)
        (spacewar:draw ship-ot2 display)
        (spacewar:update planetarium)
        (spacewar:update ship-ot1)
        (spacewar:update ship-ot2)
        (display:draw display)
      if *debug* do
        (setf (text label) (format nil "~d" (spacewar:x planetarium)))
      do (sleep 1/65))))

(defun start ()
  "Start Space clog war!."
  (initialize 'on-new-window)
  (open-browser))
