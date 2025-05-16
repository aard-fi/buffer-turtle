;;; buffer-turtle-gptel.el --- gptel bindings for turtle -*- lexical-binding: t; -*-
;;
;; Author: Bernd Wachter
;; Version: 0.1
;; Keywords: turtle, graphics
;;
;; URL: https://github.com/aard-fi/buffer-turtle
;;
;;; Commentary:
;;
;; This package provides turtle graphics bindings to gptel

;;; Code:

(require 'buffer-turtle)

(gptel-make-tool
 :function #'buffer-turtle-start-turtle
:name  "buffer-turtle-start-turtle"
:description "Start a turtle in the buffer at the turrent point."
:args (list '(:name "buffer"
                    :type string
                    :description "The buffer to place the turtle in.")
            '(:name "mode"
                    :type string
                    :description "Any value - we're drawing by hand."))
:category "emacs")

(gptel-make-tool
 :function #'buffer-turtle-kill
 :name  "buffer-turtle-kill"
 :description "Kill the poor turtle. For anothor turtle you'll need to call `buffer-turtle-start-turtle' again."
 :category "emacs")

(gptel-make-tool
 :function #'buffer-turtle-draw-left
 :name  "buffer-turtle-draw-left"
 :description "Tell the turtle started with  `buffer-turtle-start-turtle' to draw left one step."
 :category "emacs")

(gptel-make-tool
 :function #'buffer-turtle-draw-right
 :name  "buffer-turtle-draw-right"
 :description "Tell the turtle started with  `buffer-turtle-start-turtle' to draw right one step."
 :category "emacs")

(gptel-make-tool
 :function #'buffer-turtle-draw-up
 :name  "buffer-turtle-draw-up"
 :description "Tell the turtle started with  `buffer-turtle-start-turtle' to draw up one step."
 :category "emacs")

(gptel-make-tool
 :function #'buffer-turtle-draw-down
 :name  "buffer-turtle-draw-down"
 :description "Tell the turtle started with  `buffer-turtle-start-turtle' to draw down one step."
 :category "emacs")

(provide 'buffer-turtle-gptel)
;;; buffer-turtle-gptel.el ends here
