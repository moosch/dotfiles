;; splitting the root window

(defun my-split-root-window (size direction)
  (split-window (window-parent)
                (and size (prefix-numeric-value size))
                direction))

(defun split-add-window-below (&optional size)
  (interactive "P")
  (my-split-root-window size 'below))

(defun split-add-window-right (&optional size)
  (interactive "P")
  (my-split-root-window size 'right))

(defun split-add-window-left (&optional size)
  (interactive "P")
  (my-split-root-window size 'left))

