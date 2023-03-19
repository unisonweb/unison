; TLS primitives! Supplied by openssl (libssl)
#lang racket/base
(require racket/port
         racket/exn
         racket/file
         racket/unsafe/ops
         unison/tls
         unison/concurrent
         compatibility/mlist
         unison/data
         x509
         openssl)

(define key "-----BEGIN RSA PRIVATE KEY-----\nMIIEpQIBAAKCAQEAqrYLNGg7nUDgLOubYa+PlunMgLONjKhYKi/lZbQTzMPXWxko\nZ5ly++kPYrvHcJjEK2jnW4388OgAKr7Otq3qiocN0zxGqRkbL2XivviLlCHQgKOH\nBwnTbuLdnXiX3Qf2VE43ViLzY5CzthUO2fJnRuLK2wYCd/HZIMT25sJdjMzTtalV\n3Pz9fbhOtSuWdHlvAX/pi+Af7BI4x5CDieGAfy5d11hCfaRa0LqDvPvFPE47GU3O\nLuu5qJI0Mv+BiTHeYt9La2WtXWgPweeOp+qfwsssjlin3c0vlcCeCadSJG7reit0\n2Xt4kf4KquR0J5jb+gBQDhQGzyypOFiu1rEDEQIDAQABAoIBAGnkpMysf5ZLYQPc\nXrVY3txBSS7osjx3TMFWlpjg/Is/TH5g+7RP9oiXCIk5bDzHNqCq4SDk0etqLnhV\nhgrBlUS7A/NtZ3OLdFaRf/bwXDzWQO0bAy51hOc66Te+T423W7DuF9thsNKpNyES\nrPS7Lh9FnYoBj1Rx/Dsckv+Pv9IPVHUWiDLwXKs3p3tk9j1Nr9sR5dOiAML8wVxq\ngqPeQdYGGWj5PwwUL4pPakAIPTFOwzGcXP/GWUDDxG7iMM6XDIuinscQWU4703ed\nPfqIp1e3hacF1EGvla6uqfPLpuCK+O1Hxhn/wAs5MGLOMMqMmhCI/ApWCrIx28Iv\neYpdaAECgYEA4km7Uh7zH/FYHFr12pzRCLJ5cc+wD291M8FBxYgWGiE/T9EefED4\nPV3/0MrHh0oOUxHXyB/emQwzLLYJPtwRtMo23H+gZih/pgVoeqHyI00n35SDV2qW\n6oclZv4mTmTYqXsqdzYJqadQbQn19BRETxCEF+8/gD4JJRvei61yLIECgYEAwSAx\ngzHVjP9KkvHea0sUYgwJh1++9tg6YMyGenX8GrEVGYdiJUQWxeheFsD+o4kZa8h3\ndj4VNnrzrGHpXASACvMpBPw2OO1gjYh6IZUlZGu4fQn7h7rbMOkfilQreb56xhwX\nrvgPmmrJc6oFaxhKPPFs+fPVF5o/Tk7Fd1Y2zpECgYEAzN3d5mI40LGKiNlc/he9\nM/eI9Z4HEmJhJIMRjck/4VY6YIO96tZTKBPjF1+OtxA32cMsrXUU7VDbP95qjmtk\n6F2uv4AYcoS11FPPHD9j3cvrjoKBSnIowZmrJgxwF8c7VG06rATTSLI+oQlEkD6M\n/VvBi33B6tO/KcaKe8CCvoECgYEAhXhnX6FYUkq5UlcAfax02NIy37FHPDfyGKYh\nJo9V/Wh3CZ6sMM8e4gYWyKj3EzSUMg5oqXp8lJtivrcoXSTrDSZCKiTYoH1Fpms0\n5PK71ewwo5H7mTGFfQcQejxzk38WQMZ3g8ACoJi+w0Y02m/+FqJPy2s90UMUD12X\nUUNvO1ECgYEA0UhBBAiaGkL/SIqe83IJK9havTasw9qI2K+uDbh39nFQSUHqOdwg\neBweSkm6h8Dm0VNUvfGfqj/c6COEo3nrka3qRM970vLesbRup0YgCaT6GU0t1Ve8\nWqqVo1+g84KEW/rrgUiRfehp3a5k19NZ3t13MLz1Q5SzpVtUfSc+epA=\n-----END RSA PRIVATE KEY-----")
(define cert "-----BEGIN CERTIFICATE-----\nMIID4zCCAsugAwIBAgIUBa1Kv0BSVQ8x5Te3jg1d0l7IcwkwDQYJKoZIhvcNAQEL\nBQAwgYAxCzAJBgNVBAYTAlVTMQ0wCwYDVQQIDARVdGFoMRcwFQYDVQQHDA5TYWx0\nIExha2UgQ2l0eTESMBAGA1UECgwJUExULCBJbmMuMQ8wDQYDVQQDDAZsYW1iZGEx\nJDAiBgkqhkiG9w0BCQEWFW1mbGF0dEBwbHQtc2NoZW1lLm9yZzAeFw0xOTA3MjIx\nMTAyNTJaFw0yOTA3MTkxMTAyNTJaMIGAMQswCQYDVQQGEwJVUzENMAsGA1UECAwE\nVXRhaDEXMBUGA1UEBwwOU2FsdCBMYWtlIENpdHkxEjAQBgNVBAoMCVBMVCwgSW5j\nLjEPMA0GA1UEAwwGbGFtYmRhMSQwIgYJKoZIhvcNAQkBFhVtZmxhdHRAcGx0LXNj\naGVtZS5vcmcwggEiMA0GCSqGSIb3DQEBAQUAA4IBDwAwggEKAoIBAQCqtgs0aDud\nQOAs65thr4+W6cyAs42MqFgqL+VltBPMw9dbGShnmXL76Q9iu8dwmMQraOdbjfzw\n6AAqvs62reqKhw3TPEapGRsvZeK++IuUIdCAo4cHCdNu4t2deJfdB/ZUTjdWIvNj\nkLO2FQ7Z8mdG4srbBgJ38dkgxPbmwl2MzNO1qVXc/P19uE61K5Z0eW8Bf+mL4B/s\nEjjHkIOJ4YB/Ll3XWEJ9pFrQuoO8+8U8TjsZTc4u67mokjQy/4GJMd5i30trZa1d\naA/B546n6p/CyyyOWKfdzS+VwJ4Jp1Ikbut6K3TZe3iR/gqq5HQnmNv6AFAOFAbP\nLKk4WK7WsQMRAgMBAAGjUzBRMB0GA1UdDgQWBBQIWNT6zm1LQqrTa4Wrl3I2y6fF\nmDAfBgNVHSMEGDAWgBQIWNT6zm1LQqrTa4Wrl3I2y6fFmDAPBgNVHRMBAf8EBTAD\nAQH/MA0GCSqGSIb3DQEBCwUAA4IBAQAnUcZcG2Gc1LVIw6UOMoPnASy8DkMblwuk\nZivsdMhAGbNN0W15r4pzPNrFZ4+SpzJFkoDRCS4+8kVJ1Uf9uJbpLTJaEV+9ilbN\n+9kqCO1Qa2ocYcWIddOGuHqiUeNS0rGQ1/sitZHR2hrT+ynh/Pd3HRfo6h08g4Ty\nMmEtpBVrF95ypnDBjh3FHVlUW6f8g2xW8NcA6p+Ktp4fHIYWuo8DSGSn0aIkJMHy\nFnNuNxwAwtl++RiIZb/7TX6yVV97MiCsJLmWoyzpoFhC2rnv1OwUIjn6+b7Ea6ai\nuqF7U5AilSeMaLmZKmWDX0nWFMA/eOCS4A8JNaLh+XltZ7hMnvSx\n-----END CERTIFICATE-----")

(define (example-client prom)
    (let*-values (
        [(port) (promise-read prom)]
        [(input output) (ssl-connect "0.0.0.0" port)]
        [(buf) (make-bytes 200)])

        (read-bytes-avail! buf input)
        (display (bytes->string/utf-8 buf))
        (display "\n")

        ;     (display (port->string input))
        (write-string "Hello from client" output)
        (flush-output output)
        ;    (display (port->string input #:close? false))
    ))

(define (example-server prom)
  (let*-values (
    ; [(port) 9001]
    [(ctx) (ssl-make-server-context
                #:private-key (list 'pem "test.pem")
                #:certificate-chain "test.pem")]
    [(listener) (ssl-listen 0 5 #f #f ctx)]
    [(_ port __ ___) (ssl-addresses listener #t)]
    [(_) (promise-write prom port)]
    [(input output) (ssl-accept listener)]
    [(buf) (make-bytes 200)])
        (write-string "Hello from server" output)
        (flush-output output)
        (read-bytes-avail! buf input)
        (display (bytes->string/utf-8 buf))
        (display "\n")))

(define prom (promise-new))
(define thd (thread (lambda () (example-server prom))))

(example-client prom)
(thread-wait thd)
