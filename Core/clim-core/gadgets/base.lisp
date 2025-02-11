;;; ---------------------------------------------------------------------------
;;;   License: LGPL-2.1+ (See file 'Copyright' for details).
;;; ---------------------------------------------------------------------------
;;;
;;;  (c) copyright 2000 by Arthur Lemmens <lemmens@simplex.nl>
;;;  (c) copyright 2000 by Iban Hatchondo <hatchond@emi.u-bordeaux.fr>
;;;  (c) copyright 2000 by Julien Boninfante <boninfan@emi.u-bordeaux.fr>
;;;  (c) copyright 2001 by Lionel Salabartan <salabart@emi.u-bordeaux.fr>
;;;  (c) copyright 2001 by Michael McDonald <mikemac@mikemac.com>
;;;  (c) copyright 2001 by Gilbert Baumann <unk6@rz.uni-karlsruhe.de>
;;;  (c) copyright 2014 by Robert Strandh <robert.strandh@gmail.com>
;;;
;;; ---------------------------------------------------------------------------
;;;
;;; Implementation of gadgets as defined in 30.
;;;

(in-package #:clim-internals)

;;;; Notes

;;; The spec says ORIENTED-GADGET-MIXIN, we call it ORIENTED-GADGET and
;;; later define ORIENTED-GADGET-MIXIN with the remark "Try to be
;;; compatible with Lispworks' CLIM."
;;;
;;; This makes me suspect, that either "ORIENTED-GADGET-MIXIN" in the
;;; spec is a typo, or all other classes like e.g. ACTION-GADGET should
;;; really be named e.g. ACTION-GADGET-MIXIN. Also that would make more
;;; sense to me. --GB

;;; We have: LABELLED-GADGET, the spec has LABELLED-GADGET-MIXIN. Typo?
;;; Compatibility?

;;; Why is there GADGET-LABEL-TEXT-STYLE? The spec says, that just the
;;; pane's text-style should be borrowed.

;;; RANGE-GADGET / RANGE-GADGET-MIXIN: same thing as with
;;; ORIENTED-GADGET-MIXIN.

;;; Why is there no (SETF GADGET-RANGE*) in the spec? Omission?

;;; I would like to make COMPOSE-LABEL-SPACE and DRAW-LABEL* into some
;;; sort of label protocol, so that application programmers can
;;; programm their own sort of labels alleviateing the need for
;;; something like a drawn button gadget.
;;;
;;; Q: Can we make it so that a mixin class can override another mixin
;;;    class?
;;;
;;;    All the programmer should need to do is e.g.
;;;
;;;    (defclass pattern-label-mixin ()
;;;      (pattern :initarg :pattern))
;;;
;;;    (defmethod compose-label-space ((me pattern-label-mixin))
;;;      (with-slots (pattern) me
;;;        (make-space-requirement :width (pattern-width pattern)
;;;                                :height (pattern-height pattern))))
;;;
;;;    (defmethod draw-label ((me pattern-label-mixin) x1 y1 x2 y2)
;;;      (with-slots (pattern) me
;;;        (draw-design me (transform-region
;;;                         (make-translation-transformation x1 y1)
;;;                         pattern))))
;;;
;;;    (defclass patterned-button (pattern-label-mixin push-button-pane)
;;;      ())
;;;
;;; But then this probably is backwards. Specifing that :LABEL can be
;;; another pane probably is much easier and would still allow for the
;;; backend to choose the concrete widget class for us.
;;;
;;; --GB

;;; - Should RADIO-BOX-PANE and CHECK-BOX-PANE use rack or box layout?

;;; - :CHOICES initarg to RADIO-BOX and CHECK-BOX is from Franz' user
;;;   guide.

;;;; TODO

;;; - the scroll-bar needs more work:
;;;    . dragging should not change the value, the value should only
;;;      be changed after releasing the mouse.
;;;    . it should arm/disarm
;;;    . it should be deactivatable

;;; - the slider needs a total overhaul

;;; - TEXT-FILED, TEXT-AREA dito

;;; - The color of a 3Dish border should be derived from a gadget's
;;;   background.

;;; - Somehow engrafting the push button's medium does not work. The
;;;   text-style initarg does not make it to the sheets medium.

;;; - make NIL a valid label, and take it into account when applying
;;;   spacing.

;;; --------------------------------------------------------------------------
;;;
;;;  30.3 Basic Gadget Classes
;;;

;;; XXX I'm not sure that *application-frame* should be rebound like this. What
;;; about gadgets in accepting-values windows? An accepting-values window
;;; shouldn't be bound to *application-frame*. -- moore
(defun invoke-callback (pane callback &rest more-arguments)
  (when callback
    (let ((*application-frame* (pane-frame pane)))
      (apply callback pane more-arguments))))

;;; Internal protocols
(defgeneric gadget-armed-p (gadget))
(defgeneric arm-gadget (gadget))
(defgeneric disarm-gadget (gadget))

;;;
;;; gadget subclasses
;;;


;;; Labelled-gadget
#+ (or)
(progn
  (defgeneric draw-label (gadget label x y))

  (defmethod compose-space ((pane labelled-gadget) &key width height)
    (declare (ignore width height))
    (compose-space-aux pane (gadget-label pane)))

  (defmethod compose-space-aux ((pane labelled-gadget) (label string))
    (with-sheet-medium (medium pane)
      (let ((as (text-style-ascent (gadget-label-text-style pane) pane))
            (ds (text-style-descent (gadget-label-text-style pane) pane)))
        (multiple-value-bind (width height)
            (text-size medium (gadget-label pane)
                       :text-style (gadget-label-text-style pane))
          (setf height (+ as ds))
          ;; FIXME remove explicit values
          ;; instead use spacer pane in derived classes
          (let ((tw (* 1.3 width))
                (th (* 2.5 height)))
            (setf th (+ 6 height))
            (make-space-requirement :width tw :height th
                                    :max-width 400 :max-height 400
                                    :min-width tw :min-height th))))))

  (defmethod draw-label ((pane labelled-gadget) (label string) x y)
    (draw-text* pane label
                x y
                :align-x (pane-align-x pane)
                :align-y (pane-align-y pane)
                :text-style (pane-text-style pane))))


(defclass basic-gadget (;; sheet-leaf-mixin ; <- this cannot go here...
                        gadget-color-mixin
                        ;; These are inherited from pane, via
                        ;; clim-sheet-input-mixin and clim-repainting-mixin
                        ;; immediate-sheet-input-mixin
                        ;; immediate-repainting-mixin
                        basic-pane
                        gadget)
  ((id                :initarg :id                :accessor gadget-id)
   (client            :initarg :client            :accessor gadget-client)
   (armed-callback    :initarg :armed-callback    :reader gadget-armed-callback)
   (disarmed-callback :initarg :disarmed-callback :reader gadget-disarmed-callback)
   ;; I'm not so sure about the value for :initform. Maybe T is
   ;; better? Or maybe we should call ACTIVATE-GADGET after creating a
   ;; gadget? -- AL
   ;;
   ;; I think, T is correct here --GB
   (active-p          :initarg :active            :reader gadget-active-p)
   (armed             :initform nil               :reader gadget-armed-p))
  (:default-initargs :text-style (make-text-style :sans-serif nil nil)
                     :id (gensym "GADGET")
                     :client *application-frame*
                     :armed-callback nil
                     :disarmed-callback nil
                     :active t))

;;; "The default methods (on basic-gadget) call the function stored in
;;; gadget-armed-callback or gadget-disarmed-callback with one
;;; argument, the gadget."

(defmethod armed-callback ((gadget basic-gadget) client gadget-id)
  (declare (ignore client gadget-id))
  (invoke-callback gadget (gadget-armed-callback gadget)))

(defmethod disarmed-callback ((gadget basic-gadget) client gadget-id)
  (declare (ignore client gadget-id))
  (invoke-callback gadget (gadget-disarmed-callback gadget)))

;;;
;;; arming and disarming gadgets
;;;

;;; Redrawing is supposed to be handled on an :AFTER method on arm- and
;;; disarm-callback.

(defmethod arm-gadget ((gadget basic-gadget))
  (when (gadget-active-p gadget)
    (with-slots (armed) gadget
      (unless armed
        (setf armed t)
        (armed-callback gadget (gadget-client gadget) (gadget-id gadget))))))

(defmethod disarm-gadget ((gadget basic-gadget))
  (when (gadget-active-p gadget)
    (with-slots (armed) gadget
      (when armed
        (setf armed nil)
        (disarmed-callback gadget (gadget-client gadget) (gadget-id gadget))))))

;;;
;;; Activation
;;;

(defmethod activate-gadget ((gadget basic-gadget))
  (with-slots (active-p) gadget
    (unless active-p
      (setf active-p t)
      (note-gadget-activated (gadget-client gadget) gadget))))

(defmethod deactivate-gadget ((gadget basic-gadget))
  (with-slots (active-p) gadget
    (when active-p
      (disarm-gadget gadget)
      (setf active-p nil)
      (note-gadget-deactivated (gadget-client gadget) gadget))))

(defmethod note-gadget-activated (client gadget)
  ;; Default: do nothing
  (declare (ignore client gadget)))

(defmethod note-gadget-deactivated (client gadget)
  ;; Default: do nothing
  (declare (ignore client gadget)))

(defmethod note-sheet-degrafted :after ((gadget basic-gadget))
  (deactivate-gadget gadget))

(defmethod note-sheet-grafted :after ((gadget basic-gadget))
  (activate-gadget gadget))

;;;
;;; Value-gadget
;;;

(defclass value-gadget (basic-gadget)
  ((value :initarg :value
          :reader gadget-value)
   (value-changed-callback :initarg :value-changed-callback
                           :reader gadget-value-changed-callback))
  (:default-initargs :value nil
                     :value-changed-callback nil))

(defmethod reinitialize-instance :after
    ((gadget value-gadget) &key (value nil value-p))
  (when value-p
    (setf (gadget-value gadget :invoke-callback t) value)))

(defmethod (setf gadget-value) (value (gadget value-gadget) &key invoke-callback)
  (declare (ignore invoke-callback))
  (setf (slot-value gadget 'value) value))

;;; Try to call the callback after all other (setf gadget-value) methods have
;;; been called. The gadget methods, which may update graphical appearance,
;;; will be run even if the callback does a non-local exit (like throwing a
;;; presentation).

(defmethod (setf gadget-value) :around (value (gadget value-gadget)
                                        &key invoke-callback)
  (multiple-value-prog1 (call-next-method)
    (when invoke-callback
      (value-changed-callback gadget
                              (gadget-client gadget)
                              (gadget-id gadget)
                              value))))

(defmethod value-changed-callback ((gadget value-gadget) client gadget-id value)
  (declare (ignore client gadget-id))
  (invoke-callback gadget (gadget-value-changed-callback gadget) value))

;;;
;;; Action-gadget
;;;

(defclass action-gadget (basic-gadget)
  ((activate-callback :initarg :activate-callback
                      :reader gadget-activate-callback))
  (:default-initargs :activate-callback nil))

(defmethod activate-callback ((gadget action-gadget) client gadget-id)
  (declare (ignore client gadget-id))
  (invoke-callback gadget (gadget-activate-callback gadget)))

;;;
;;; Oriented-gadget
;;;

(defclass oriented-gadget ()
  ((orientation :type    (member :vertical :horizontal)
                :initarg :orientation
                :reader  gadget-orientation))
  (:default-initargs :orientation :horizontal))

(defclass oriented-gadget-mixin (oriented-gadget)
  ;; Try to be compatible with Lispworks' CLIM.
  ())

;;;
;;; Labelled-gadget
;;;

(defclass labelled-gadget ()
  ((label       :initarg :label
                :initform ""
                :accessor gadget-label)))

(defclass labelled-gadget-mixin (labelled-gadget)
  ;; Try to be compatible with Lispworks' CLIM.
  ())

;;;
;;; Range-gadget
;;;

(defclass range-gadget ()
  ((min-value :initarg :min-value :accessor gadget-min-value)
   (max-value :initarg :max-value :accessor gadget-max-value))
  (:default-initargs :min-value 0
                     :max-value 1))

(defmethod reinitialize-instance :after ((instance range-gadget)
                                         &key (min-value nil min-value-p)
                                              (max-value nil max-value-p))
  (when min-value-p
    (setf (gadget-min-value instance) min-value))
  (when max-value-p
    (setf (gadget-max-value instance) max-value)))

(defclass range-gadget-mixin (range-gadget)
  ;; Try to be compatible with Lispworks' CLIM.
  ())

(defmethod (setf gadget-min-value) :after ((new-value t) (gadget range-gadget))
  (when (< (gadget-value gadget) new-value)
    (setf (gadget-value gadget :invoke-callback t) new-value)))

(defmethod (setf gadget-max-value) :after ((new-value t) (gadget range-gadget))
  (when (> (gadget-value gadget) new-value)
    (setf (gadget-value gadget :invoke-callback t) new-value)))

(defmethod gadget-range ((gadget range-gadget))
  (- (gadget-max-value gadget)
     (gadget-min-value gadget)))

(defmethod gadget-range* ((gadget range-gadget))
  (values (gadget-min-value gadget)
          (gadget-max-value gadget)))
