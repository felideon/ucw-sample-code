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

(defcomponent books-window (standard-window-component)
  ()
  (:default-initargs
      :title "Lisp Books"))

(defcomponent order-form ()
  ())

(define-symbol-macro $username (get-session-value :username))

(defmethod render :before ((form order-form))
  (<:h1 (<:as-html "Book Order Form"))
  (<:h3 (<:as-html (format nil "Welcome, ~a" $username))))

(defmethod render :wrapping ((form order-form))
  (<:form :method "post" :action "mailto:felideon+blog@gmail.com"
	  (call-next-method)))

(defmethod render ((form order-form))
  (<:as-html "Address: ") (<:text :name "Address") (<:br)
  (<:as-html "Phone: ") (<:text :name "Phone") (<:br)
  (<:p) (render (make-instance 'products-dropdown))
  (<:p)
  (<:submit :value "Place Order"))

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

(defcomponent welcome-screen ()
  ())

(defmethod render ((welcome welcome-screen))
  (<:form :method "post" :action "index.ucw"
	  (<:h2 (<:as-html "Enter your name"))
	  (<:text :name "username")
	  (<:submit :value "Enter")))

(defun render-order-window (body-component-name &rest initargs)
  (render 
   (make-instance 'books-window  
		  :body (apply #'make-instance body-component-name initargs))))

(defentry-point "index.ucw" (:application *orders-ucw-application*
					  :with-call/cc nil)
    ((username nil))
  (when username
    (setf $username username))
  (if $username
      (render-order-window 'order-form)
      (render-order-window 'welcome-screen)))