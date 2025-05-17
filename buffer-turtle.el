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
  "The timer for the turtle")

(defvar buffer-turtle-pen-down t
  "Status of the turtles pen")

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
  (when (and (not (eobp)) (> (length char) 0))
    (delete-char (length char)))
  (insert char))

(defun buffer-turtle--draw-turtle (&optional direction)
  "Do the actual drawing"
  (when buffer-turtle-buffer
    (with-current-buffer buffer-turtle-buffer
      (let ((d (or direction buffer-turtle-instruction)))
        (cond
         ((string-equal d "stop") t)
         ((string-equal d "right")
          (buffer-turtle--insert-at-point "-"))
         ((string-equal d "left")
          (let ((col (current-column)))
            (when (> col 1)
              (goto-char (- (point) 2))
              (buffer-turtle--insert-at-point "-"))))
         ((string-equal d "up")
          (let ((col (current-column)))
            (forward-line -1)
            (move-to-column col t)
            (goto-char (- (point) 1)))
          (buffer-turtle--insert-at-point "|"))
         ((string-equal d "down")
          (let ((col (current-column)))
            (when (eq (line-number-at-pos) (line-number-at-pos (point-max)))
              (goto-char (point-max))
              (insert "\n"))
            (forward-line 1)
            (move-to-column col t)
            (goto-char (- (point) 1)))
          (buffer-turtle--insert-at-point "|")))))))
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
  (setq buffer-turtle-buffer buffer)
  (when (and mode (string-equal mode "timer"))
    (setq buffer-turtle-timer (run-at-time "2 sec" 1 'buffer-turtle--draw-turtle))))

(defun buffer-turtle-left ()
  "Make the turle move left"
  (interactive)
  (setq buffer-turtle-instruction "left"))

(defun buffer-turtle-right ()
  "Make the turtle move right"
  (interactive)
  (setq buffer-turtle-instruction "right"))

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

(defun buffer-turtle-draw-left ()
  "Make the turle draw left"
  (interactive)
  (buffer-turtle--draw-turtle "left"))

(defun buffer-turtle-draw-right ()
  "Make the turtle draw right"
  (interactive)
  (buffer-turtle--draw-turtle "right"))

(defun buffer-turtle-draw-up ()
  "Make the turtle draw up"
  (interactive)
  (buffer-turtle--draw-turtle "up"))

(defun buffer-turtle-draw-down ()
  "Make the turtle draw down"
  (interactive)
  (buffer-turtle--draw-turtle "down"))

(defun buffer-turtle-kill ()
  "Shoot the turtle and bury the corpse"
  (interactive)
  (when buffer-turtle-timer
    (cancel-timer buffer-turtle-timer)
    (setq buffer-turtle-buffer nil)
    (setq buffer-turtle-buffer nil)))

;; (buffer-turtle-left)
;; (buffer-turtle-right)
;; (buffer-turtle-up)
;; (buffer-turtle-down)
;; (buffer-turtle-stop)

(provide 'buffer-turtle)
;;; buffer-turtle.el ends here
