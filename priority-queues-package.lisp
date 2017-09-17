;;; -*- Mode: Lisp -*-

;;; priority-queues-package.lisp --
;;; Priority Queues as Binary Heaps in the CLR style (Cormen,
;;; Leiserson and Rivest, "Introduction to Algorithms", ppgg.
;;; 140--152, MIT Press).
;;; Package definition file (in CLtL2 style).
;;;
;;; Author: Marco Antoniotti
;;; Address: Robotics Laboratory
;;;          Courant Institute of Mathematical Science
;;;          New York University
;;;          New York, NY, 10012
;;;
;;; Copyright (c) 1992. All rights reserved.
;;;
;;; Version: 1.0 gamma

;;;============================================================================
;;; General License Agreement and Lack of Warranty
;;;
;;; See companion file 'priority-queues.lisp'


;;;============================================================================
;;; History:
;;; 12.21.1992: released.


(defpackage "PRIORITY-QUEUES" (:use "COMMON-LISP" "CONDITIONS")
  (:nicknames "HEAP" "PQ")
  (:export make-priority-queue
	   make-heap
	   priority-queue-p
	   heap-p
	   element-type
	   adjustable-priority-queue-p
	   adjustable-heap-p
	   priority-queue-name
	   heap-name
	   priority-queue-allocation
	   heap-allocation
	   empty-p
	   insert
	   head
	   size
	   extract-head
	   delete-by-key
	   modify-key
	   empty-error
	   unequal-keys
	   overflow
	   duplicate-key
	   version
	   )
  )

(in-package "PRIORITY-QUEUES")

(declaim (inline head
		 size
		 empty-p
		 element-type
		 adjustable-priority-queue-p
		 adjustable-heap-p
		 priority-queue-name
		 heap-name))

;;; end of file -- priority-queues-package.lisp --
