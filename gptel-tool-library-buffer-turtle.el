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
(require 'gptel-tool-library)

(defvar gptel-tool-library-buffer-turtle-tools '()
  "The list of buffer turtle related tools")

(defvar gptel-tool-library-buffer-turtle-tools-maybe-safe '()
  "The list of buffer turtle related tools which may be destructive, but typically
the LLM behaves.")

(defvar gptel-tool-library-buffer-turtle-tools-unsafe '()
  "The list of buffer related related tools which are not safe.")

(add-to-list 'gptel-tool-library-buffer-turtle-tools
             (gptel-make-tool
              :function #'buffer-turtle-start-turtle
              :name  "turtle-start"
              :description "Start a turtle in the buffer at the turrent point. You can now draw something with the movement commands. The canvas will resize automatically. Small drawings usually don't work, without good reason don't draw things smaller than 10x10. You can read the buffer contents - check the result in between steps or at the end, and clear the buffer and start again if the drawing is bad."
              :args (list '(:name "buffer"
                                  :type string
                                  :description "The buffer to place the turtle in.")
                          '(:name "mode"
                                  :type string
                                  :description "Any value - we're drawing by hand."))
              :category "buffer-turtle"))

(add-to-list 'gptel-tool-library-buffer-turtle-tools
             (gptel-make-tool
              :function #'buffer-turtle-kill
              :name  "turtle-kill"
              :description "Kill the poor turtle. For anothor turtle you'll need to call `buffer-turtle-start-turtle' again."
              :category "buffer-turtle"))

(add-to-list 'gptel-tool-library-buffer-turtle-tools
             (gptel-make-tool
              :function #'buffer-turtle-draw-left
              :name  "buffer-turtle-draw-left"
              :description "Tell the turtle started with  `buffer-turtle-start-turtle' to draw left one step."
              :category "buffer-turtle"))

(add-to-list 'gptel-tool-library-buffer-turtle-tools
             (gptel-make-tool
              :function #'buffer-turtle-draw-left-up
              :name  "buffer-turtle-draw-left-up"
              :description "Tell the turtle started with  `buffer-turtle-start-turtle' to draw left and up one step."
              :category "buffer-turtle"))

(add-to-list 'gptel-tool-library-buffer-turtle-tools
             (gptel-make-tool
              :function #'buffer-turtle-draw-left-down
              :name  "buffer-turtle-draw-left-down"
              :description "Tell the turtle started with  `buffer-turtle-start-turtle' to draw left and down one step."
              :category "buffer-turtle"))

(add-to-list 'gptel-tool-library-buffer-turtle-tools
             (gptel-make-tool
              :function #'buffer-turtle-draw-right
              :name  "buffer-turtle-draw-right"
              :description "Tell the turtle started with  `buffer-turtle-start-turtle' to draw right one step."
              :category "buffer-turtle"))

(add-to-list 'gptel-tool-library-buffer-turtle-tools
             (gptel-make-tool
              :function #'buffer-turtle-draw-right-up
              :name  "buffer-turtle-draw-right-up"
              :description "Tell the turtle started with  `buffer-turtle-start-turtle' to draw right and up one step."
              :category "buffer-turtle"))

(add-to-list 'gptel-tool-library-buffer-turtle-tools
             (gptel-make-tool
              :function #'buffer-turtle-draw-right-down
              :name  "buffer-turtle-draw-right-down"
              :description "Tell the turtle started with  `buffer-turtle-start-turtle' to draw right and down one step."
              :category "buffer-turtle"))

(add-to-list 'gptel-tool-library-buffer-turtle-tools
             (gptel-make-tool
              :function #'buffer-turtle-draw-up
              :name  "buffer-turtle-draw-up"
              :description "Tell the turtle started with  `buffer-turtle-start-turtle' to draw up one step."
              :category "buffer-turtle"))

(add-to-list 'gptel-tool-library-buffer-turtle-tools
             (gptel-make-tool
              :function #'buffer-turtle-draw-down
              :name  "buffer-turtle-draw-down"
              :description "Tell the turtle started with  `buffer-turtle-start-turtle' to draw down one step."
              :category "buffer-turtle"))

(add-to-list 'gptel-tool-library-buffer-turtle-tools
             (gptel-make-tool
              :function #'buffer-turtle-pen-down
              :name  "buffer-turtle-pen-down"
              :description "Put the pen down (i.e., paint on movement)."
              :category "buffer-turtle"))

(add-to-list 'gptel-tool-library-buffer-turtle-tools
             (gptel-make-tool
              :function #'buffer-turtle-pen-up
              :name  "buffer-turtle-pen-up"
              :description "Put the pen up (i.e., don't paint on movement)."
              :category "buffer-turtle"))

(provide 'gptel-tool-library-buffer-turtle)
;;; buffer-turtle-gptel.el ends here
