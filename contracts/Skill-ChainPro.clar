;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SkillChain Pro
;; Decentralized Trade Apprenticeship Certification System
;; Built for Stacks Blockchain
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ERROR CODES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-constant err-owner-only (err u100))
(define-constant err-not-authorized (err u101))
(define-constant err-program-not-found (err u102))
(define-constant err-program-inactive (err u103))
(define-constant err-already-enrolled (err u104))
(define-constant err-not-enrolled (err u105))
(define-constant err-not-completed (err u106))
(define-constant err-certificate-not-found (err u107))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DATA VARIABLES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-data-var contract-owner principal tx-sender)

(define-data-var program-id-counter uint u0)
(define-data-var certificate-id-counter uint u0)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DATA MAPS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Trade Programs
(define-map programs
  { program-id: uint }
  {
    name: (string-ascii 100),
    description: (string-ascii 300),
    instructor: principal,
    active: bool
  }
)

;; Apprentices enrolled in programs
(define-map enrollments
  { apprentice: principal, program-id: uint }
  {
    completed: bool,
    completion-height: uint
  }
)

;; Certificates issued
(define-map certificates
  { certificate-id: uint }
  {
    apprentice: principal,
    program-id: uint,
    issued-at: uint
  }
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PRIVATE FUNCTIONS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-private (is-owner)
  (is-eq tx-sender (var-get contract-owner))
)

(define-private (is-instructor (program-id uint))
  (let ((program (map-get? programs { program-id: program-id })))
    (if (is-some program)
        (is-eq tx-sender (get instructor (unwrap-panic program)))
        false
    )
  )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PUBLIC FUNCTIONS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ---------------------------------------------------
;; 1. Create Trade Program (Owner Only)
;; ---------------------------------------------------

(define-public (create-program
    (name (string-ascii 100))
    (description (string-ascii 300))
    (instructor principal)
  )
  (begin
    (asserts! (is-owner) err-owner-only)
    (asserts! (not (is-eq instructor tx-sender)) err-owner-only)
    (asserts! (> (len name) u0) err-owner-only)
    (asserts! (> (len description) u0) err-owner-only)

    (var-set program-id-counter (+ (var-get program-id-counter) u1))

    (map-set programs
      { program-id: (var-get program-id-counter) }
      {
        name: name,
        description: description,
        instructor: instructor,
        active: true
      }
    )

    (ok (var-get program-id-counter))
  )
)

;; ---------------------------------------------------
;; 2. Deactivate Program (Owner Only)
;; ---------------------------------------------------

(define-public (deactivate-program (program-id uint))
  (let ((program (map-get? programs { program-id: program-id })))
    (asserts! (is-owner) err-owner-only)
    (asserts! (> program-id u0) err-owner-only)
    (asserts! (is-some program) err-program-not-found)

    (map-set programs
      { program-id: program-id }
      {
        name: (get name (unwrap-panic program)),
        description: (get description (unwrap-panic program)),
        instructor: (get instructor (unwrap-panic program)),
        active: false
      }
    )

    (ok true)
  )
)

;; ---------------------------------------------------
;; 3. Enroll in Program
;; ---------------------------------------------------

(define-public (enroll (program-id uint))
    (begin
      (asserts! (> program-id u0) err-owner-only)
    (let (
      (program (map-get? programs { program-id: program-id }))
      (existing (map-get? enrollments { apprentice: tx-sender, program-id: program-id }))
     )
    (asserts! (is-some program) err-program-not-found)
    (asserts! (get active (unwrap-panic program)) err-program-inactive)
    (asserts! (is-none existing) err-already-enrolled)

    (map-set enrollments
      { apprentice: tx-sender, program-id: program-id }
      {
        completed: false,
        completion-height: u0
      }
    )

    (ok true)
  )
    )
)

;; ---------------------------------------------------
;; 4. Mark Completion (Instructor Only)
;; ---------------------------------------------------

(define-public (mark-completed (apprentice principal) (program-id uint))
  (begin
    (asserts! (not (is-eq apprentice tx-sender)) err-owner-only)
    (asserts! (> program-id u0) err-owner-only)
  (let ((enrollment (map-get? enrollments { apprentice: apprentice, program-id: program-id })))
    (asserts! (is-instructor program-id) err-not-authorized)
    (asserts! (is-some enrollment) err-not-enrolled)

    (map-set enrollments
      { apprentice: apprentice, program-id: program-id }
      {
        completed: true,
        completion-height: stacks-block-height
      }
    )

    (ok true)
  )
  )
)

;; ---------------------------------------------------
;; 5. Issue Certificate (Instructor Only)
;; ---------------------------------------------------

(define-public (issue-certificate (apprentice principal) (program-id uint))
  (begin
    (asserts! (not (is-eq apprentice tx-sender)) err-owner-only)
    (asserts! (> program-id u0) err-owner-only)
  (let ((enrollment (map-get? enrollments { apprentice: apprentice, program-id: program-id })))
    (asserts! (is-instructor program-id) err-not-authorized)
    (asserts! (is-some enrollment) err-not-enrolled)
    (asserts! (get completed (unwrap-panic enrollment)) err-not-completed)

    (var-set certificate-id-counter (+ (var-get certificate-id-counter) u1))

    (map-set certificates
      { certificate-id: (var-get certificate-id-counter) }
      {
        apprentice: apprentice,
        program-id: program-id,
        issued-at: stacks-block-height
      }
    )

    (ok (var-get certificate-id-counter))
  )
  )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; READ-ONLY FUNCTIONS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Get program details
(define-read-only (get-program (program-id uint))
  (map-get? programs { program-id: program-id })
)

;; Get enrollment details
(define-read-only (get-enrollment (apprentice principal) (program-id uint))
  (map-get? enrollments { apprentice: apprentice, program-id: program-id })
)

;; Get certificate
(define-read-only (get-certificate (certificate-id uint))
  (map-get? certificates { certificate-id: certificate-id })
)
