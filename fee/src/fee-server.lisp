;;; -*- Mode: Lisp; Syntax: COMMON-LISP; Package: jana.metamodel; Base: 10 -*-  
;;; -*- coding:utf-8 -*-
;;;***************************************************************************
;;;FILE:               fee-server.lisp
;;;LANGUAGE:           Common-Lisp
;;;
;;;DESCRIPTION
;;;    
;;; 
;;;
;;;AUTHORS
;;;    <CHR> Christian Hofmann <c.hofmann@utwente.nl>
;;;
;;;MODIFICATIONS
;;;    2008-12-04 <CHR> Initial Version.
;;;
;;; LEGAL
;;;    
;;;    Copyright C. Hofmann 2009
;;;    mailto:c.hofmann@utwente.nl
;;;
;;; This code is free software; you can redistribute it and/or
;;; modify it under the terms of the version 2.1 of
;;; the GNU Lesser General Public License as published by 
;;; the Free Software Foundation, as clarified by the AllegroServe
;;; prequel found in license-allegroserve.txt.
;;;
;;; This code is distributed in the hope that it will be useful,
;;; but without any warranty; without even the implied warranty of
;;; merchantability or fitness for a particular purpose.  See the GNU
;;; Lesser General Public License for more details.
;;;
;;; Version 2.1 of the GNU Lesser General Public License is in the file 
;;; license-lgpl.txt that was distributed with this file.
;;; If it is not present, you can access it from
;;; http://www.gnu.org/copyleft/lesser.txt (until superseded by a newer
;;; version) or write to the Free Software Foundation, Inc., 59 Temple Place, 
;;; Suite 330, Boston, MA  02111-1307  USA
;;;    
;;;***************************************************************************


(in-package :FEE)

;; requires: jana-java/src/java-project.lisp
;;           (mk:operate-on-system 'portable 'load)
;;           (mk:operate-on-system 'foil 'load)

(defclass server ()
  ((server-address
    :ACCESSOR server-address
    :INITARG :address
    :TYPE string
    :DOCUMENTATION "The IP-address or name of a server. Example: \"127.0.0.1\" \"localhost\"")
   (server-port
    :ACCESSOR server-port
    :INITARG :port
    :TYPE fixnum
    :DOCUMENTATION "A server port. Example: 81"))
  (:DOCUMENTATION "A server provides a remote connection."))

(defclass foil-server (server)
  ((foreign-vm
    :ACCESSOR foreign-vm
    :INITARG :fvm
    :DOCUMENTATION "The foreign Virtual Machine to which we connect using Foil."))
  (:DOCUMENTATION "A connection to a server on a remote (Java) Virtual Machine that provides the 'Foil' Foreign-Object Interface for Lisp."))

(defun make-foil-server (&optional (host-name "localhost") (port-nr 13579))
  "Constructor"
  (make-instance 'foil-server :address host-name :port port-nr))

(defmethod connect-to-foil-server ((self foil-server))
  "Establishes the connection to the remote VM using the Foil foreign-object interface."
  (setf (foreign-vm self)
	(make-instance 'foil:foreign-vm
		       :stream
		       (portable-sockets:open-connection
			(server-address self)
			(server-port self))))
  (setq foil:*fvm* (foreign-vm self)))

(defclass fee-server (foil-server)
  ((server-instance
    :ACCESSOR server-instance
    :INITARG :server
    :DOCUMENTATION "A handle to the remote fee-server object."))
  (:DOCUMENTATION "A proxy class for the interface provided by the Java class fee.FeeServer."))

(defun make-fee-server (&optional (host-name "localhost") (port-nr 13579))
  "Constructor.
RETURNS a  new fee-server instance."
  (make-instance 'fee-server :address host-name :port port-nr))

(defmethod connect-to-fee-server ((self fee-server) &key
				  ((:print-banner print-banner) t))
  "Establishes the connection to the remote VM using the Foil foreign-object interface,
 and binds a fee.FeeServer instance to (server-instance SELF)."
  ;; the macro definitions of foil try to immediately evaluate the
  ;; macros in the context of a connected VM.
  ;; At load time, however there is not yet a connection.
  ;; Therefore, all definitions and function calls through Foil
  ;; have to be protected. Definitions have to be protected from
  ;; macroexpansion, and function calls from evaluation through the reader,
  ;; as the packages are created at run-time.
  (connect-to-foil-server self)
  (eval '(foil:def-foil-class "fee.FeeServer"))
  (setf (server-instance self)
	(funcall (read-from-string "|fee|::feeserver.getinstance")))
  (when print-banner
    (eval '(foil::def-foil-class "javax.swing.JOptionPane"))
    (funcall (read-from-string "|javax.swing|::joptionpane.showmessagedialog") nil "Hello Fee!")))

(defmethod set-compact-mode ((self fee-server))
  (funcall (read-from-string "|fee|::feeserver.setCompactMode")
	   (server-instance self)))

(defmethod set-project-jar-file ((self fee-server) jar-file-name)
  "Adds the file-name of a JAR file to the analysis class-path."
  (funcall (read-from-string "|fee|::feeserver.setProjectJarFile")
	   (server-instance self)
	   jar-file-name))

(defmethod analyze ((self fee-server) (class-names list))
  "Analyzes a list of classes given by CLASS-NAMES in parallel."
  (format t "Analyzing Classes: ~A" class-names)
  (dolist (class-name class-names)
    (analyze self class-name)))
  ; chr: FIX THIS!
  ;(funcall (read-from-string "|fee|::feeserver.analyze")
  ;(server-instance self)
  ;class-names))

(defmethod analyze ((self fee-server) (class-name string))
  "Analyzes a class whose fully qualified name is given by the
string CLASS-NAME."
  (funcall (read-from-string "|fee|::feeserver.analyze")
	   (server-instance self)
	   class-name))

(defmethod analyze-aspects-in-project ((self fee-server) project-instance) ;(project-instance jana.metamodel::project))
  "Analyzes only those class-files of a project that contain aspects."
  (set-project-jar-file self
			(jar-file project-instance))
  (if (aspect-names project-instance)
      (analyze self
	       (jana.java:aspect-names project-instance))
      (format t "Warning! No aspects found in project!" )))

(defmethod analyze-project ((self fee-server) project-instance) ; jana.metamodel::project))
  "Analyzes all class-files that are part of project.
This includes classes and aspects."
  (analyze-aspects-in-project self project-instance)
  (if (jana.java:class-names project-instance)
      (analyze self
	       (jana.java:class-names project-instance))
      (error "The project contains no classes!")))

