(in-package #:ucw)

(defun make-orders-backend ()
  (make-backend :httpd :host "localhost" :port 8080))

(defclass orders-server (standard-server)
  ())

(defun make-orders-server ()
  (make-instance 'orders-server :backend (make-orders-backend)))

(defvar *orders-ucw-server* (make-orders-server))

(defclass orders-application (basic-application)
  ()
  (:default-initargs
   :url-prefix "/orders/"))

(defparameter *orders-ucw-application*
  (make-instance 'orders-application))

(register-application *orders-ucw-server* *orders-ucw-application*)

(defun startup-orders ()
  (startup-server *orders-ucw-server*))

(defun shutdown-orders ()
  (shutdown-server *orders-ucw-server*))

(defcomponent order-window (standard-window-component)
  ()
  (:default-initargs
      :title "Book Order Form"
    :body (make-instance 'form-component)))

(defcomponent form-component ()
  ())

(defmethod render ((form form-component))
  (<:form :method "post" :action "mailto:felideon+blog@gmail.com"
	  (<:as-html "Name: ") (<:text :name "Name") (<:br)
	  (<:as-html "Address: ") (<:text :name "Address") (<:br)
	  (<:as-html "Phone: ") (<:text :name "Phone") (<:br)
	  (<:p) (render (make-instance 'products-dropdown))
	  (<:p) (<:submit :value "Place Order")))

(defcomponent products-dropdown ()
  ())

(defmethod render ((products products-dropdown))
  (<:select :name "Product"
   (<:option :value "PCL" "Practical Common Lisp")
   (<:option :value "C@W" "Coders At Work")
   (<:option :value "OOPCLOS"
	     "OOP in Common Lisp: A Programmer's Guide to CLOS")
   (<:option :value "AMOP" "The Art of the Metaobject Protocol")
   (<:option :value "GENTLE"
	     "Common Lisp: A Gentle Introduction to Symbolic Computation")))

(defentry-point "index.ucw" (:application *orders-ucw-application*
					  :with-call/cc nil)
    ()
  (render (make-instance 'order-window)))