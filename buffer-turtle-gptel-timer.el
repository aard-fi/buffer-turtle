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
:description "Start a turtle in the buffer with the command 'stop' at the current point. The turtle will check and execute the current command roughly once per second."
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
 :function #'buffer-turtle-right
 :name  "buffer-turtle-right"
 :description "Tell the turtle started with  `buffer-turtle-start-turtle' in timer mod to move right until it receives another command."
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

(provide 'buffer-turtle-gptel-timer)
;;; buffer-turtle-gptel-timer.el ends here
