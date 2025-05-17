;;; buffer-turtle-gptel-timer.el --- gptel bindings for turtle -*- lexical-binding: t; -*-
;;
;; Author: Bernd Wachter
;; Version: 0.1
;; Keywords: turtle, graphics
;;
;; URL: https://github.com/aard-fi/buffer-turtle
;;
;;; Commentary:
;;
;; This package provides turtle graphics bindings to gptel, with the turtle
;; being triggered by a timer, and following the current instruction until
;; changed.

;;; Code:

(require 'buffer-turtle)

(gptel-make-tool
 :function #'buffer-turtle-start-turtle
:name  "buffer-turtle-start-turtle"
:description "Start a turtle in the buffer with the command 'stop' at the current point. The turtle will check and execute the current command roughly once per second. You can now give instructions to the turtle to execute until you provide a new instruction. The canvas will resize automatically. Small drawings usually don't work, without good reason don't draw things smaller than 10x10. You can read the buffer contents - check the result in between steps or at the end, and clear the buffer and start again if the drawing is bad."
:args (list '(:name "buffer"
                    :type string
                    :description "The buffer to place the turtle in.")
            '(:name "mode"
                    :type string
                    :description "`timer' to have the turtle check for instructions every second. Anything else for manual drawing"))
:category "emacs")

(gptel-make-tool
 :function #'buffer-turtle-left
 :name  "buffer-turtle-left"
 :description "Tell the turtle started with  `buffer-turtle-start-turtle' in timer mode to move left until it receives another command."
 :category "emacs")

(gptel-make-tool
 :function #'buffer-turtle-left-up
 :name  "buffer-turtle-left-up"
 :description "Tell the turtle started with  `buffer-turtle-start-turtle' in timer mode to move left and up until it receives another command."
 :category "emacs")

(gptel-make-tool
 :function #'buffer-turtle-left-down
 :name  "buffer-turtle-left-down"
 :description "Tell the turtle started with  `buffer-turtle-start-turtle' in timer mode to move left and down until it receives another command."
 :category "emacs")

(gptel-make-tool
 :function #'buffer-turtle-right
 :name  "buffer-turtle-right"
 :description "Tell the turtle started with  `buffer-turtle-start-turtle' in timer mod to move right until it receives another command."
 :category "emacs")

(gptel-make-tool
 :function #'buffer-turtle-right-up
 :name  "buffer-turtle-right-up"
 :description "Tell the turtle started with  `buffer-turtle-start-turtle' in timer mod to move right and up until it receives another command."
 :category "emacs")

(gptel-make-tool
 :function #'buffer-turtle-right-down
 :name  "buffer-turtle-right-down"
 :description "Tell the turtle started with  `buffer-turtle-start-turtle' in timer mod to move right and down until it receives another command."
 :category "emacs")

(gptel-make-tool
 :function #'buffer-turtle-up
 :name  "buffer-turtle-up"
 :description "Tell the turtle started with  `buffer-turtle-start-turtle' in timer mode to move up until it receives another command."
 :category "emacs")

(gptel-make-tool
 :function #'buffer-turtle-down
 :name  "buffer-turtle-down"
 :description "Tell the turtle started with  `buffer-turtle-start-turtle' in timer mode to move down until it receives another command."
 :category "emacs")

(gptel-make-tool
 :function #'buffer-turtle-stop
 :name  "buffer-turtle-stop"
 :description "Tell the turtle started with  `buffer-turtle-start-turtle' to not move until it receives another command."
:category "emacs")

(gptel-make-tool
 :function #'buffer-turtle-kill
 :name  "buffer-turtle-kill"
 :description "Kill the poor turtle. For anothor turtle you'll need to call `buffer-turtle-start-turtle' again."
 :category "emacs")

(gptel-make-tool
 :function #'buffer-turtle-pen-down
 :name  "buffer-turtle-pen-down"
 :description "Put the pen down (i.e., paint on movement)."
 :category "emacs")

(gptel-make-tool
 :function #'buffer-turtle-pen-up
 :name  "buffer-turtle-pen-up"
 :description "Put the pen up (i.e., don't paint on movement)."
 :category "emacs")

(gptel-make-tool
 :function #'buffer-turtle-clear-buffer
 :name  "buffer-turtle-clear-buffer"
 :description "Clear the turtle buffer so you can start over."
 :category "emacs")

(gptel-make-tool
 :function #'buffer-turtle-read-buffer
 :name  "buffer-turtle-read-buffer"
 :description "Return the content of the turtle buffer so you can verify your progress."
 :category "emacs")

(provide 'buffer-turtle-gptel-timer)
;;; buffer-turtle-gptel-timer.el ends here
