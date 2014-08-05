(import (net server) 
	(rfc http-server)
	(sagittarius socket) (pp))


(define server
  (http-server (make-simple-server-socket "5000")
	       on-error: (lambda (e) (print e))
	       ("/"  (lambda (method path headers in)
		       (pp headers)
		       (values "200" '() "OK")))
	       ("/*" (lambda (method path headers in)
		       (pp headers)
		       (values "200" '() "Hello world")))))

(run-server server)