;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; rfc/http-server.scm - HTTP server
;;;  
;;;   Copyright (c) 2014  Takashi Kato  <ktakashi@ymail.com>
;;;   
;;;   Redistribution and use in source and binary forms, with or without
;;;   modification, are permitted provided that the following conditions
;;;   are met:
;;;   
;;;   1. Redistributions of source code must retain the above copyright
;;;      notice, this list of conditions and the following disclaimer.
;;;  
;;;   2. Redistributions in binary form must reproduce the above copyright
;;;      notice, this list of conditions and the following disclaimer in the
;;;      documentation and/or other materials provided with the distribution.
;;;  
;;;   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;;;   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;;;   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;;;   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
;;;   OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;;;   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
;;;   TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
;;;   PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
;;;   LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;;   NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;;   SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;;  

(library (rfc http-server)
    (export make-http-server http-server?
	    http-server
	    add-http-handler!)
    (import (rnrs)
	    (srfi :1 lists)
	    (srfi :13 strings)
	    (srfi :106 socket)
	    (net server)
	    (prefix (binary io) binary:)
	    (util bytevector)
	    (util port))

  (define-record-type (%http-server %make-http-server http-server?)
    (parent server)
    (fields (mutable handlers))
    (protocol
     (lambda (n)
       (lambda (disp handler)
	 ((n disp) handler)))))

  (define (error-page-handler . ignore)
    (values "404" '() "Page not found"))

  (define (http-handler server socket)
    (define *eol* #vu8(#x0d #x0a))
    (define (read-http-request in)
      ;; should we accept \n?
      (let* ((first (utf8->string (binary:get-line in :eol *eol*)))
	     (tokens (string-tokenize first)))
	(unless (or (= (length tokens) 3))
	  (error 'http-handler "bad http request" first))
	(let loop ((line (binary:get-line in :eol *eol*))
		   (r '()))
	  (if (zero? (bytevector-length line))
	      (values (car tokens) (caddr tokens) (cadr tokens) (reverse! r))
	      (loop (binary:get-line in :eol *eol*)
		    (cons (utf8->string line) r))))))
    (define (match-handler handlers path)
      (let loop ((handlers handlers))
	(cond ((null? handlers) error-page-handler)
	      ;; best match
	      ((string=? (caar handlers) path) (cdar handlers))
	      ;; registered path = /* request /something
	      ((and (string-suffix? "*" (caar handlers))
		    (string-prefix? (string-drop-right (caar handlers) 1) path))
	       (cdar handlers))
	      (else (loop (cdr handlers))))))

    (define (write-headers out headers)
      ;; TODO
      ;; write default headers

      ;; write handler headers
      )

    (let ((in (socket-input-port socket))
	  (out (socket-output-port socket)))
      (guard (e (else (put-bytevector out (string->utf8 "HTTP 500 /"))
		      (put-bytevector out *eol*)
		      (flush-output-port out)
		      (raise e)))
	(let-values (((method version path header) (read-http-request in)))
	  (let ((handler (match-handler (%http-server-handlers server) path)))
	    (let-values (((status headers contents)
			  (handler method path header in)))
	      (put-bytevector out (string->utf8 
				   (string-append "HTTP " status " " path)))
	      (put-bytevector out *eol*)
	      (write-headers out headers)
	      (put-bytevector out *eol*)
	      (cond ((bytevector? contents) (put-bytevector out contents))
		    ((string? contents) 
		     (put-bytevector out (string->utf8 contents)))
		    ((port? contents) (copy-binary-port out contents))
		    (else
		     ;; what shall we do with the drunken salier...
		     (put-bytevector out (string->utf8 "[Internal error]"))
		     (error 'http-handler "invalid content" contents)))
	      (flush-output-port out)))))))

  (define (make-http-server socket-creator handlers :key (error-handler #f))
    (make-simple-server socket-creator http-handler
			:make-server %make-http-server
			:error-handler error-handler
			handlers))

  (define (add-http-handler! server path handler)
    (let ((handlers (%http-server-handlers server)))
      (%http-server-handlers-set! server (alist-cons path handler handlers))))

  (define-syntax http-server
    (syntax-rules (on-error:)
      ((_ "parse" socket eh ((p h) ...) ((path proc) rest ...))
       (http-server "parse" socket eh ((p h) ... (path proc)) (rest ...)))
      ((_ "parse" socket eh ((p h) ...) ())
       (make-http-server socket (list (cons p h) ...) :error-handler eh))
      ;; entry points
      ((_ socket on-error: eh specs ...)
       (http-server "parse" socket eh () (specs ...)))
      ((_ socket specs ...)
       (http-server socket on-error: #f specs ...))))

)