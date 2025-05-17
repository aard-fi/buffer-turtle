;;; buffer-turtle.el --- Simple turtle graphics in a buffer -*- lexical-binding: t; -*-
;;
;; Author: Bernd Wachter
;; Version: 0.1
;; Keywords: turtle, graphics
;;
;; URL: https://github.com/aard-fi/buffer-turtle
;;
;;; Commentary:
;;
;; This package provides basic turtle graphics functionality
;; within an Emacs buffer. You can command the turtle to move
;; left, right, up, down, or stop, and it will draw simple
;; lines accordingly.

;;; Code:

(defvar buffer-turtle-buffer nil
  "The current buffer occupied by the turtle.")

(defvar buffer-turtle-instruction "stop"
  "The current instruction issued to the turtle.")

(defvar buffer-turtle-timer nil
  "The timer for the turtle.")

(defvar buffer-turtle-pen-down t
  "Status of the turtles pen.")

(defvar buffer-turtle-log-movements t
  "Log movements of the turtle. Especially when using an LLM to control the
turtle this is quite useful for following along.")

(defvar buffer-turtle-show-turtle nil
  "Show the turtle at point. Disable for now due to broken rendering.")

(defvar buffer-turtle-last-position nil
  "Details of the previous painting position")

(defun buffer-turtle--insert-top-lines (num)
  "Insert NUM lines at the top of the buffer"
  (save-excursion
    (goto-char (point-min))
    (insert (make-string num ?\n))))

(defun buffer-turtle--insert-bottom-lines (num)
  "Insert NUM lines at the bottom of the buffer"
  (save-excursion
    (goto-char (point-max))
    (insert (make-string num ?\n))))

(defun buffer-turtle--insert-left-lines (num)
  "Insert NUM vertical lines at the left side"
  (save-excursion
    (goto-char (point-min))
    (let ((padding (make-string num ?\s)))
      (while (not (eobp))
        (insert padding)
        (forward-line 1)))))

(defun buffer-turtle--insert-at-point (char)
  "Replace the character at point with our turtle character"
  (when (and (not (eobp)) (not (eolp)) (> (length char) 0))
    (delete-char (length char)))
  (insert char))

(defun buffer-turtle--draw-at (char x y)
  "Draw at a fixed position without moving point"
  (when buffer-turtle-log-movements
    (message (format "Restoring at [%s/%s]" x y)))
  (save-excursion
    (goto-char (point-min))
    (forward-line y)
    (move-to-column x t)
    (buffer-turtle--insert-at-point char)))

(defun buffer-turtle--draw (char x y)
  "Draw at a fixed position with moving point, increasing canvas as needed."
  (when buffer-turtle-log-movements
    (message (format "Moving turtle [%s/%s] -> [%s/%s]" (current-column) (line-number-at-pos) x y)))
  ;; drawing char at last point of turtle before adjusting canvas
  ;; simplifies things
  (when buffer-turtle-last-position
    (buffer-turtle--draw-at
     (plist-get buffer-turtle-last-position :c)
     (plist-get buffer-turtle-last-position :x)
     (plist-get buffer-turtle-last-position :y)))
  (when (> y (line-number-at-pos (point-max)))
    (buffer-turtle--insert-bottom-lines (- y (line-number-at-pos (point-max)))))
  (when (< y 1)
    (let ((padding (+ (* y -1) 1)))
      (buffer-turtle--insert-top-lines padding)
      (setq y (+ y padding))))
  (when (< x 0)
    (let ((padding (* x -1)))
      (buffer-turtle--insert-left-lines padding)
      (setq x (+ x padding))))
  (goto-char (point-min))
  (forward-line (- y 1))
  (move-to-column x t)
  (if buffer-turtle-show-turtle
      (progn
        (setq buffer-turtle-last-position
              `(:x ,(current-column)
                   :y ,(line-number-at-pos)
                   :c ,(if buffer-turtle-pen-down char " ")))
        (buffer-turtle--insert-at-point "x"))
    (buffer-turtle--insert-at-point char)))

(defun buffer-turtle--move-turtle (&optional direction)
  "Do the actual drawing"
  (when buffer-turtle-buffer
    (with-current-buffer buffer-turtle-buffer
      (let ((d (or direction buffer-turtle-instruction))
            (x (current-column))
            (y (line-number-at-pos))
            (c nil))
        (cond
         ((string-equal d "stop") t)
         ((string-equal d "right")
          (setq c `(:x ,x :y ,y :c "·")))
         ((string-equal d "right-up")
          (setq c `(:x ,x :y ,(- y 1) :c "·")))
         ((string-equal d "right-down")
          (setq c `(:x ,x :y ,(+ y 1) :c "·")))
         ((string-equal d "left")
          (setq c `(:x ,(- x 2) :y ,y :c "·")))
         ((string-equal d "left-up")
          (setq c `(:x ,(- x 2) :y ,(- y 1) :c "·")))
         ((string-equal d "left-down")
          (setq c `(:x ,(- x 2) :y ,(+ y 1) :c "·")))
         ((string-equal d "up")
          (setq c `(:x ,(- x 1 ):y ,(- y 1) :c "·")))
         ((string-equal d "down")
          (setq c `(:x ,(- x 1) :y ,(+ y 1) :c "·"))))
        (buffer-turtle--draw (plist-get c :c)
                                (plist-get c :x)
                                (plist-get c :y))))))

(defun buffer-turtle-pen-up ()
  "Move the pen up"
  (interactive)
  (setq buffer-turtle-pen-down nil))

(defun buffer-turtle-pen-down ()
  "Move the pen down"
  (interactive)
  (setq buffer-turtle-pen-down t))

(defun buffer-turtle-start-turtle (buffer &optional mode)
  "Start turtle painting in the given buffer.

If mode is specified as `timer' it ill check every second if instructions have
changed, otherwise the turtle needs to be controlled with manual draw commands"
  (interactive)
  (when buffer-turtle-timer
    (buffer-turtle-kill))
  (setq buffer-turtle-buffer (get-buffer-create buffer))
  (buffer-turtle-pen-down)
  (when (and mode (string-equal mode "timer"))
    (setq buffer-turtle-timer (run-at-time "2 sec" 1 'buffer-turtle--move-turtle))))

;;; timer movement instructions
(defun buffer-turtle-left ()
  "Make the turle move left"
  (interactive)
  (setq buffer-turtle-instruction "left"))

(defun buffer-turtle-left-up ()
  "Make the turle move left and up"
  (interactive)
  (setq buffer-turtle-instruction "left-up"))

(defun buffer-turtle-left-down ()
  "Make the turle move left and down"
  (interactive)
  (setq buffer-turtle-instruction "left-down"))

(defun buffer-turtle-right ()
  "Make the turtle move right"
  (interactive)
  (setq buffer-turtle-instruction "right"))

(defun buffer-turtle-right-up ()
  "Make the turtle move right and up"
  (interactive)
  (setq buffer-turtle-instruction "right-up"))

(defun buffer-turtle-right-down ()
  "Make the turtle move right and down"
  (interactive)
  (setq buffer-turtle-instruction "right-down"))

(defun buffer-turtle-up ()
  "Make the turtle move up"
  (interactive)
  (setq buffer-turtle-instruction "up"))

(defun buffer-turtle-down ()
  "Make the turtle move down"
  (interactive)
  (setq buffer-turtle-instruction "down"))

(defun buffer-turtle-stop ()
  "Stop the turtle"
  (interactive)
  (setq buffer-turtle-instruction "stop"))

;;; direct movement commandos
(defun buffer-turtle-draw-left ()
  "Make the turle draw left"
  (interactive)
  (buffer-turtle--move-turtle "left"))

(defun buffer-turtle-draw-left-up ()
  "Make the turle draw left and up"
  (interactive)
  (buffer-turtle--move-turtle "left-up"))

(defun buffer-turtle-draw-left-down ()
  "Make the turle draw left and down"
  (interactive)
  (buffer-turtle--move-turtle "left-down"))

(defun buffer-turtle-draw-right ()
  "Make the turtle draw right"
  (interactive)
  (buffer-turtle--move-turtle "right"))

(defun buffer-turtle-draw-right-up ()
  "Make the turtle draw right and up"
  (interactive)
  (buffer-turtle--move-turtle "right-up"))

(defun buffer-turtle-draw-right-down ()
  "Make the turtle draw right and down"
  (interactive)
  (buffer-turtle--move-turtle "right-down"))

(defun buffer-turtle-draw-up ()
  "Make the turtle draw up"
  (interactive)
  (buffer-turtle--move-turtle "up"))

(defun buffer-turtle-draw-down ()
  "Make the turtle draw down"
  (interactive)
  (buffer-turtle--move-turtle "down"))

(defun buffer-turtle-kill ()
  "Shoot the turtle and bury the corpse"
  (interactive)
  (when buffer-turtle-timer
    (cancel-timer buffer-turtle-timer)
    (setq buffer-turtle-buffer nil)
    (setq buffer-turtle-buffer nil)))

(defun buffer-turtle-draw-square (length)
  "Test function for drawing a simple square"
  (dotimes (i length)
    (buffer-turtle-draw-right))
  (dotimes (i length)
    (buffer-turtle-draw-down))
  (dotimes (i length)
    (buffer-turtle-draw-left))
  (dotimes (i length)
    (buffer-turtle-draw-up)))

(defun buffer-turtle-draw-negative-square (length)
  "Test function for drawing a simple square most likely requiring canvas resizes"
  (dotimes (i length)
    (buffer-turtle-draw-left))
  (dotimes (i length)
    (buffer-turtle-draw-up))
  (dotimes (i length)
    (buffer-turtle-draw-right))
  (dotimes (i length)
    (buffer-turtle-draw-down)))

(defun buffer-turtle-draw-hexagon (length)
  "Test function for drawing a simple square"
  (dotimes (i length)
    (buffer-turtle-draw-right))
  (dotimes (i length)
    (buffer-turtle-draw-right-down))
  (dotimes (i length)
    (buffer-turtle-draw-down))
  (dotimes (i length)
    (buffer-turtle-draw-left-down))
    (dotimes (i length)
    (buffer-turtle-draw-left))
  (dotimes (i length)
    (buffer-turtle-draw-left-up))
  (dotimes (i length)
    (buffer-turtle-draw-up))
  (dotimes (i length)
    (buffer-turtle-draw-right-up)))

;; (buffer-turtle-start-turtle "*scratch*")
;; (buffer-turtle-left)
;; (buffer-turtle-right)
;; (buffer-turtle-up)
;; (buffer-turtle-down)
;; (buffer-turtle-stop)
;; (buffer-turtle-pen-down)
;; (buffer-turtle-pen-up)
;; (buffer-turtle-draw-left)
;; (buffer-turtle-draw-right)
;; (buffer-turtle-draw-up)
;; (buffer-turtle-draw-down)
;; (buffer-turtle-draw-square 10)
;; (buffer-turtle-draw-negative-square 10)
;; (buffer-turtle-draw-hexagon 10)

(provide 'buffer-turtle)
;;; buffer-turtle.el ends here
