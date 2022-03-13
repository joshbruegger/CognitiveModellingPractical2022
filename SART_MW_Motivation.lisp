;;  Sustained Attention to Response Task (SART)
;;
;;  In each trial the participant sees a letter: "O" or "Q".
;;  They must press a key every time an O appears (90% of trials),
;;  but withhold their response when the stimulus is a Q (10% of trials).
;;
;;  Cognitive Modelling Practical 2021
;;  Updated for ACT-R 7.21 by Loran Knol


;;===================;;
;;  Experiment code  ;;
;;===================;;


;; Experiment settings
(defvar *stimulus-duration* 2) ; number of seconds the stimulus is shown
(defvar *inter-stimulus-interval* 0.5) ; number of seconds between trials
(defvar *target-trials* 180) ; number of target trials
(defvar *non-target-trials* 20) ; number of non-target trials

(defvar *output-directory* "~/output/") ; location where output files are stored
(defvar *trace-to-file-only* nil) ; whether the model trace should only be saved to file and not appear in terminal
(defvar *trace-file-name* "sart-trace") ; name of file in which the trace is stored

(defvar *terminal-stream* *standard-output*) ; necessary for stream management

(defvar *visible* nil) ; visibility of the experiment window

;; Global variables for data storage
(defvar *stimuli* nil)
(defvar *trial-response* nil)
(defvar *trial-start* nil)
(defvar *trial-rt* nil)
(defvar *trial-done* nil)
(defvar *all-responses* nil)
(defvar *all-rts* nil)

;; Parallellism management
(defvar *lock* (bt:make-lock))

;; Do SART experiment n times, save trace to output file
(defun do-sart-n (n)
  (with-open-file
    (*file-stream*
      (ensure-directories-exist
        (merge-pathnames
          (make-pathname :name *trace-file-name* :type "txt")
          *output-directory*))
      :direction :output :if-does-not-exist :create :if-exists :supersede)

  (if *trace-to-file-only*
    ; If true, direct standard output only file
    (setf *standard-output* *file-stream*)
    ; Else, direct standard output to terminal and file
    (setf *standard-output*
      (make-broadcast-stream *terminal-stream* *file-stream*)))

  ; Direct ACT-R output to the stream contained within *standard-output*
  (echo-act-r-output)

  (setf *visible* nil)
  (format t "Running ~a model participants~%" n)
  (dotimes (i n)
    (let ((participant (1+ i)))
      (format t "Run ~a...~%" participant)
      (do-sart)
      (write-results-to-file
        (concatenate 'string "dat" (write-to-string participant))
        participant
        *stimuli*
        (reverse *all-responses*)
        (reverse *all-rts*))))
  (format t "Done~%")

  ; We will close *file-stream* now, so make sure *standard-output*
  ; no longer points to it
  (setf *standard-output* *terminal-stream*)
  ; We also have to make sure ACT-R knows about the new value of
  ; *standard-output*
  (echo-act-r-output)
  )
)

;; Do SART experiment 1 time
(defun do-sart ()
  (reset)
  (setf *all-responses* nil)
  (setf *all-rts* nil)
  (setf *stimuli*
    (permute-list
      (concatenate
        'list
        (make-array *target-trials* :initial-element "O")
        (make-array *non-target-trials* :initial-element "Q"))))
  (setf *visible* nil)
  (loop for stim in *stimuli* do (run-trial stim))
)

;; Do a single SART trial with a target stimulus
(defun do-sart-trial-o ()
  (setf *visible* t)
  (run-trial "O")
)

;; Do a single SART trial with a non-target stimulus
(defun do-sart-trial-q ()
  (setf *visible* t)
  (run-trial "Q")
)


;; Execute a trial with a given stimulus
(defun run-trial (stim)
  (let ((window (open-exp-window "SART Experiment"
                               :visible *visible*
                               :width 300
                               :height 300
                               :x 300
                               :y 300)))

    (add-text-to-exp-window window
                            stim
                            :width 30
                            :height 30
                            :x 145
                            :y 150)

  (add-act-r-command
    "sart-key-press"
    'key-event-handler
    "SART task key press monitor")
  (monitor-act-r-command "output-key" "sart-key-press")

  (setf *trial-response* nil)
  (setf *trial-start* (get-time))
  (setf *trial-rt* nil)
  (setf *trial-done* nil)

  (install-device window)
  (run-full-time *stimulus-duration* *visible*)
  (clear-exp-window)
  (run-full-time *inter-stimulus-interval* *visible*)

  (remove-act-r-command-monitor "output-key" "sart-key-press")
  (remove-act-r-command "sart-key-press"))

  ; Prevent race conditions
  (bt:with-lock-held
    (*lock*)
    (push *trial-response* *all-responses*)
    (push *trial-rt* *all-rts*))
)

;; Register the model's key presses (ignore the model parameter)
(defun key-event-handler (model key)
  (declare (ignore model))

  ; Prevent race conditions
  (bt:with-lock-held
  (*lock*)
    (setf *trial-rt* (/ (- (get-time) *trial-start*) 1000.0))
    (setf *trial-response* (string key))
    (setf *trial-done* t))
)

;; Write the behavioural results to a file
(defun write-results-to-file (name participant stimuli responses rts)
  (with-open-file
    (out
      (ensure-directories-exist
        (merge-pathnames
          (make-pathname :name name :type "csv")
          *output-directory*))
      :direction :output :if-does-not-exist :create :if-exists :supersede)
    (format out "participant, trial, stimulus, response, rt~%")
    (loop
      for trial from 1
      for stimulus in stimuli
      for response in responses
      for rt in rts
      do (format out "~a, ~a, ~a, ~a, ~a~%" participant trial stimulus response rt)))
)



;;===================;;
;;    Model code     ;;
;;===================;;

(clear-all)

(defvar matchingReward)
(setq matchingReward 0.2)

(defvar trialEndReward)
(setq trialEndReward -0.2)

(defvar checkGoalReward)
(setq checkGoalReward -0.0005)

(defvar snapBackRewardValue)
(setq snapBackRewardValue 4)

(defvar decreasingFactor)
(setq decreasingFactor 0.5)

;; Custom reward only for attend
(defun give-reward (reward)
  (let ((newReward (+ reward (car (car (spp attend :u)))))
        (command "(spp attend :u ")
  )

  (setq command (concatenate 'string command (format nil "~A" newReward)))
  (setq command (concatenate 'string command ")"))
  (eval (read-from-string command))

  (format t "REWARD GIVEN: ~a, NEW UTILITY: ~a" reward (car(car(spp attend :u))))
  
  )
)

(defun snapBackReward ()
  (setq snapBackRewardValue (- snapBackRewardValue decreasingFactor))
  (cond ((< snapBackRewardValue 0.5)
   0)
   (t snapBackRewardValue))
)

(defun reward-hook-fun (id)

  (cond ( (string-equal (evt-details id) "PRODUCTION-FIRED CHECK-CURRENT-GOAL")
          (format t "GIVING REWARD FOR CHECK-CURRENT-GOAL~%")
          (give-reward checkGoalReward)
        )
        ( (string-equal (evt-details id) "PRODUCTION-FIRED RETRIEVE-RESPONSE")
          (format t "GIVING REWARD FOR RETRIEVE-RESPONSE~%")
          (give-reward matchingReward)
        )
        ( (string-equal (evt-details id) "PRODUCTION-FIRED PREPARE-FOR-NEXT")
          (format t "GIVING REWARD FOR PREPARE-FOR-NEXT~%")
          (give-reward trialEndReward) 
        )
        ( (string-equal (evt-details id) "PRODUCTION-FIRED REMEMBER-TO-ATTEND")
          (format t "GIVING REWARD FOR REMEMBER-T0-ATTEND~%")
          (give-reward (snapBackReward))
        )
        ( (string-equal (evt-details id) "PRODUCTION-FIRED RETRIEVE-MEMORY")
          (format t "MIND WANDERING! UTILITY: ~a" (car(car(spp attend :u))))
        )
  ) 
)

(add-post-event-hook 'reward-hook-fun)

(define-model sart

;; Model parameters
(sgp :v t ; main trace detail
  :act low ; activation trace detail
  :sact t ; include activation trace in main trace

  :show-focus t ; show where the model is looking
  :esc t ; enable sub-symbolic level
  :rt -5 ; retrieval threshold
  :bll 0.5 ; base-level learning
  :ans 0.2 ;activation noise
  :epl t ; enable prouction-compilation
  :pct t ; production compilation trace
  :ul t ; Utility learning required for production comp
  :ult t; utility learning trace flag
  :egs 0.01 ; Utiliy noise
)

(chunk-type beginning label)
(chunk-type goal state)
(chunk-type subgoal step)
(chunk-type srmapping stimulus hand)
(chunk-type memory content)

(add-dm
  (start isa chunk)
  (press-on-O isa srmapping stimulus "O" hand left)
  (withhold-on-Q isa srmapping stimulus "Q" hand nil)
  (attend isa goal state attend)
  (wander isa goal state wander)
  (identify isa subgoal step identify)
  (get-response isa subgoal step get-response)
  (make-response isa subgoal step make-response)
  (prepare-for-next isa subgoal step prepare-for-next)
  

  ;; Memories
  (memory1 isa memory content "cute cat")
  (memory2 isa memory content "cute dog")
  (memory3 isa memory content "cute bunny")
  (memory4 isa memory content "cute fox")
  (memory5 isa memory content "cute otter")
  (memory6 isa memory content "cute seal")
  (memory7 isa memory content "cute fox")
  (memory8 isa memory content "cute girl")
  (memory9 isa memory content "cute bee")
  (memory10 isa memory content "memory of memory")
  (memory11 isa memory content "the thing you can't stop thinking of")
  (memory12 isa memory content "remember when you did that embarrassing thing")
  (memory13 isa memory content "cute boy")
  (memory14 isa memory content "crush")
  (memory15 isa memory content "assignments")
  (memory16 isa memory content "did you turn off the stove?")
  (memory17 isa memory content "call your mom")
  (memory18 isa memory content "what was the name of that one song")
  (memory19 isa memory content "never gonna give you up")
  (memory20 isa memory content "I want pizza")
  (memory21 isa memory content "Simon the cat video")
  (memory22 isa memory content "Stephen Jones' brilliant accent")
  (memory23 isa memory content "Can you breathe on mars")
  (memory24 isa memory content "Cognitive modelling practical assignment")
  (memory25 isa memory content "TAs you're great pls give us good grade")
  (memory26 isa memory content "if im 70kg and I eat 10kg of pizza, am I 8% pizza")
  (memory27 isa memory content "what will I have for diner?")
  (memory28 isa memory content "what will I have for diner?")
  (memory29 isa memory content "what will I have for diner?")
  (memory30 isa memory content "what will I have for diner?")
  (memory31 isa memory content "what will I have for diner?")
  (memory32 isa memory content "what will I have for diner?")
  (memory33 isa memory content "what will I have for diner?")
  (memory34 isa memory content "what will I have for diner?")
  (memory35 isa memory content "what will I have for diner?")
  (memory36 isa memory content "what will I have for diner?")
  (memory37 isa memory content "what will I have for diner?")
  (memory38 isa memory content "what will I have for diner?")
  (memory39 isa memory content "what will I have for diner?")
  (memory40 isa memory content "what will I have for diner?")
  (memory41 isa memory content "what will I have for diner?")
  (memory42 isa memory content "what will I have for diner?")
  (memory43 isa memory content "what will I have for diner?")
  (memory44 isa memory content "what will I have for diner?")
  (memory45 isa memory content "what will I have for diner?")
  (memory46 isa memory content "what will I have for diner?")
  (memory47 isa memory content "what will I have for diner?")
  (memory48 isa memory content "what will I have for diner?")
  (memory49 isa memory content "what will I have for diner?")
  (memory50 isa memory content "what will I have for diner?")
  (memory51 isa memory content "what will I have for diner?")
  (memory52 isa memory content "what will I have for diner?")
  (memory53 isa memory content "what will I have for diner?")
  (memory54 isa memory content "what will I have for diner?")
  (memory55 isa memory content "what will I have for diner?")
  (memory56 isa memory content "what will I have for diner?")
  (memory57 isa memory content "what will I have for diner?")
  (memory58 isa memory content "what will I have for diner?")
  (memory59 isa memory content "what will I have for diner?")
  (memory60 isa memory content "what will I have for diner?")
  (memory61 isa memory content "what will I have for diner?")


  (remember-to-attend isa memory content "I should attend")
)

(set-base-levels
  (press-on-O    10000  -10000)
  (withhold-on-Q  10000  -10000)

  (memory1  200 -10000) (remember-to-attend  220 -10000)
  (memory2  200 -10000) (memory3  200 -10000)
  (memory4  200 -10000) (memory5  200 -10000)
  (memory6  200 -10000) (memory7  200 -10000)
  (memory8  200 -10000) (memory9  200 -10000)

  (memory10 200 -10000) (memory11 200 -10000)
  (memory12 200 -10000) (memory13 200 -10000)
  (memory14 200 -10000) (memory15 200 -10000)
  (memory16 200 -10000) (memory17 200 -10000)
  (memory18 200 -10000) (memory19 200 -10000)

  (memory20 200 -10000) (memory21 200 -10000)
  (memory22 200 -10000) (memory23 200 -10000)
  (memory24 200 -10000) (memory25 200 -10000)
  (memory26 200 -10000) (memory27 200 -10000)
  (memory28 200 -10000) (memory29 200 -10000)

  (memory30 200 -10000) (memory31 200 -10000)
  (memory32 200 -10000) (memory33 200 -10000)
  (memory34 200 -10000) (memory35 200 -10000)
  (memory36 200 -10000) (memory37 200 -10000)
  (memory38 200 -10000) (memory39 200 -10000)

  (memory40 200 -10000) (memory41 200 -10000)
  (memory42 200 -10000) (memory43 200 -10000)
  (memory44 200 -10000) (memory45 200 -10000)
  (memory46 200 -10000) (memory47 200 -10000)
  (memory48 200 -10000) (memory49 200 -10000)

  (memory50 200 -10000) (memory51 200 -10000)
  (memory52 200 -10000) (memory53 200 -10000)
  (memory54 200 -10000) (memory55 200 -10000)
  (memory56 200 -10000) (memory57 200 -10000)
  (memory58 200 -10000) (memory59 200 -10000)
  (memory60 200 -10000) (memory61 200 -10000)
)

(p attend
  ?goal>
    buffer    empty
==>
  +goal>
    isa       goal
    state     attend
)
(spp attend :u 5)

(p wander
  ?goal>
    buffer              empty
==>
  +retrieval>
    isa                 memory
  - content             nil
  - content             "I should attend"
    :recently-retrieved nil
  -imaginal>
  +goal>
    isa                 goal
    state               wander
)
(spp wander :u 0)

(p check-current-goal
  =goal>
    isa           goal
    state         attend
  ?visual-location>
    buffer        empty
  ?visual>
  - scene-change  T
==>
  -goal>
)

(p identify-stimulus
  =goal>
    isa       goal
    state     attend
  =visual-location>
  ?visual>
    state       free
==>
  +visual>
    isa         move-attention
    screen-pos  =visual-location
  +goal>
    isa         subgoal
    step        get-response
)

(p retrieve-response
  =goal>
    isa       subgoal
    step      get-response
  =visual>
    isa       text
    value     =letter
  ?visual>
    state     free
  ;; ?retrieval>
  ;;   state     free
==>
  +retrieval>
    isa       srmapping
    stimulus  =letter
  +goal>
    isa       subgoal
    step      make-response
  +visual>
    isa       clear-scene-change
)
(spp retrieve-response :u 0.2)

(p respond-if-O
  =goal>
    isa       subgoal
    step      make-response
  =retrieval>
    isa       srmapping
  ;; - stimulus  nil
    hand      =hand
  ?manual>
    state     free
==>
  +manual>
    isa       punch
    hand      =hand
    finger    index
  =goal>
    isa       subgoal
    step      prepare-for-next

)

(p dont-respond-if-Q
  =goal>
    isa       subgoal
    step      make-response
  =retrieval>
    isa       srmapping
    hand      nil
  ;;- stimulus  nil
  ?manual>
    state     free
==>
  =goal>
    isa       subgoal
    step      prepare-for-next
)

(p prepare-for-next
  =goal>
    isa       subgoal
    step      prepare-for-next
==>
  -goal>
  -visual-location>
  -visual>
)

(spp prepare-for-next :reward t)

(p retrieve-memory
  =goal>
    state               wander
  ?retrieval>
    state               free
    buffer              empty
==>
  +retrieval>
    isa                 memory
  - content             nil
    :recently-retrieved nil
  -imaginal>
)

(p attend-memory
  =goal>
    state               wander
  =retrieval>
  - content             "I should attend"
    content             =memory
  ?imaginal>
    state               free
==>
  +imaginal>
    isa                 memory
    content             =memory
  =retrieval>
    state               nil
  -retrieval>
)

(p respond-standard
  =goal>
    state               wander
  =visual-location>
  ?retrieval>
    state               free
    buffer              empty
  ?manual>
    state     free
==>
  +manual>
    isa       punch
    hand      left
    finger    index
  -visual-location>
)
(spp respond-standard :u 0.5)

(p remember-to-attend
  =goal>
    state               wander
  =retrieval>
    isa                 memory
    content             "I should attend"
  ==>
  =retrieval>
    state               attend
    content             nil
  -retrieval>
  +retrieval>
    isa                 goal
    state               attend
  -goal>
  -imaginal>
)

)
