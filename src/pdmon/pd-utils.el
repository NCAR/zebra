;;
;; Utilities for dealing with plot descriptions.
;;
(defvar pd::rcsid "$Id: pd-utils.el,v 1.2 1993-02-25 17:20:21 corbet Exp $"
 "The RCS id")
;
; Our position when scanning through the PD.
;
(defvar pd::position 0 "Scanning position")
(make-variable-buffer-local 'pd::position)
(defvar pd::plot-type nil "The plot type")
(make-variable-buffer-local 'pd::plot-type)

;
; To add:
;
; markup functions
; correctness checking
;


(defun pd::check () "Check whether this is a decent PD or not"
	(interactive)
;
; Save the current position, then return then when all is done.  This is
; not done via save-excursion or some such because if an error is signaled
; we want to stay where things went wrong.
;
	(setq pd::position (point-min))
	(let ((saved-pt (point)))
		(pd::reset-zones)
		(setq pd::plot-type
			(downcase
			    (or (pd::retrieve "global" "plot-type") "none")))
		(pd::do-check)
		(goto-char saved-pt)
		(message "Check went OK")
	)
)



(defun pd::do-check () "Actually perform the pd check"
    (goto-char (point-min))
    (let ((state 'initial) line)
    	(while (setq line (pd::get-line))
	    (cond
	    ;
	    ; Enforce the "global" component as the first.
	    ;
	    	((eq state 'initial) 
		    (progn
		    	(pd::check-global line)
			(setq pd::component "global")
			(pd::mark-line (point) pd::comp-style)
			(setq state 'begin-comp)))
	    ;
	    ; If this looks like another component name, do a couple
	    ; of quick checks.
	    ;
	    	((and (/= (string-to-char line) ?\t) (eq state 'begin-comp))
		    (pd::gripe "Empty component"))
	    	((/= (string-to-char line) ?\t)
		    (progn
		    	(pd::check-comp-name line)
			(setq pd::component line)
			(pd::mark-line (point) pd::comp-style)
			(setq state 'in-comp)))
	     ;
	     ; Otherwise this had better be a component.
	     ;
	     	((pd::check-param line) (setq state 'in-comp))
		(t (pd::gripe "Unrecognized PD line"))
	    )
	)
    )
)



;
; line-by-line access to the PD.
;
(defun pd::get-line () "Get the next line from this PD, removing comments"
    (goto-char pd::position)
    (let ((lbegin (point)) ret)
    	(cond
	;
	; If at the end return nil.
	;
	    ((eobp) nil)
	;
	; Trim out comments here.
	;
	    ((eq (following-char) ?!)
	        (progn
			(end-of-line)
			(or (eobp) (forward-char 1))
			(setq pd::position (point))
			(pd::get-line)
		))
	;
	; Failing all that, extract a line.
	;
	    (t (progn
	    	(end-of-line)
		(setq ret (buffer-substring lbegin (point)))
		(or (eobp) (forward-char 1))
		(setq pd::position (point))
		(goto-char lbegin)
		ret))
	)
    )
)





(defun pd::check-global (line) "See if this is the global component"
	(or (string-equal (downcase line) "global")
	    (string-equal (downcase line) "defaults") ; xxx
		(pd::gripe "PD must start with global component"))
)





;
; Enforce some conventions on component names.
;
(defun pd::check-comp-name (name) "Check a component name"
	(if (not (string-match "^[a-zA-Z0-9_.\-]+$" name))
		(pd::gripe "Bad component name"))
)


;
; Perform checking on PD parameters.
;
(defun pd::check-param (line) "Check a component line"
    (let (rule)
	(cond
	    ((null (string-match "^\t+[a-zA-Z0-9_\-]+:[ \t]+.*$" line)) nil)
	    ((setq rule (pd::prop-find-rule (pd::pname line))) 
	    	(pd::rule-check (pd::pvalue line) rule) t)
	    (t (pd::mark-line (point) pd::unk-param-style) t)
	)
    )
)





;
; Extract the parameter name from this line.
;
(defun pd::pname (line) "Extract param name from line"
    (string-match "[^ \t]*:" line) ; assume good line
    (substring line (match-beginning 0) (- (match-end 0) 1))
)


;
; Extract the parameter value from this line.
;
(defun pd::pvalue (line) "Extract param name from line"
    (string-match ":[ \t]+" line) ; assume good line
    (substring line (match-end 0))
)



;
; Retrieval.
;
(defun pd::retrieve (comp param) "Retrieve a PD parameter"
    (save-excursion (let ((limits (pd::limits comp)))
	(if limits
	    (pd::find-param (car limits) (cdr limits) param)
	    nil
	)
    ))
)


(defun pd::find-param (begin end param) "Find the parameter in this comp"
    (goto-char begin)
    (if (re-search-forward (concat "^\t+" param ":[\t ]+") end t)
        (progn
	    (end-of-line)
	    (buffer-substring (match-end 0) (point))
	)
	nil
    )
)




;
; Return a dotted pair delimiting the given plot description component.
;
(defun pd::limits (name) "Find the extent of this component"
    (let ((exp (concat "^" name "$")))
        (save-excursion
	    (goto-char (point-min))
	    (if (re-search-forward exp nil t)
	    	(cons (match-beginning 0)
		    (if (re-search-forward "^[^\t]" nil t)
		    	(- (match-beginning 0) 1) (point-max)))
		nil
	    )
	)
    )
)




;
; Complain about an erroneous line.
;
(defun pd::gripe (complaint) "Put out a gripe"
	(epoch::clear-zones)
	(pd::mark-line (point) pd::gripe-style)
	(if (eq (following-char) ?\t)
		(re-search-forward ":[ \t]*"))
	(error complaint)
)



;;
;; Herein starts the rule table for plot description parameters.
;;
;; Format:
;;
;;	( parameter  plot-type  representation  check-function
;;	  description  check-data)
;;
;; The plot-type and representation, if non-nil, restrict the application
;; of this rule to situations where they match.
;;
;; At this point, with the (much faster) property-list-based rule lookup
;; scheme, the plot-type and representation are not used for rule matches.
;; If it is worthwhile, that capability can be restored at some point.
;;

(defmacro pd::r-param (rule) (list 'nth '0 rule))
(defmacro pd::r-ptype (rule) (list 'nth '1 rule))
(defmacro pd::r-rep (rule) (list 'nth '2 rule))
(defmacro pd::r-check (rule) (list 'nth '3 rule))
(defmacro pd::r-descr (rule) (list 'nth '4 rule))
(defmacro pd::r-cdata (rule) (list 'nth '5 rule))

(setq pd::ParamRules '(
	("age-limit"	nil		nil		pd::ck-interval
	 "The oldest that data for this overlay is allowed to be before
being dropped.  Qualified by platform name.")

	("altitude"	"cap"		nil		pd::ck-float
	 "The altitude of the constant altitude plane, in km.  If this
window is operating in radar-space, this parameter is instead the elevation
of the radar, in degrees." 
	)

	("annotation-format"	nil	nil	nil
	 "A printf-style format for side annotation for track plots.  This
parameter (qualified by field name) is mostly useful for fields with a range
where the default %.2f format does not make sense.")

	("annot-height"		"cap"	"overlay"	pd::ck-float
	 "The height of grid overlay annotation as a portion of the height
of the whole window.")

	("arrow"		"cap"	"track"		pd::ck-bool
	 "Determines whether wind vectors are added onto an aircraft track.")

	("arrow-color"		"cap"	"track"		nil
	 "The color to use in drawing arrows on tracks.")

	("arrow-interval"	"cap"	"track"		pd::ck-interval
	 "The time interval at which arrows are placed along an aircraft
track, if the ARROW parameter is TRUE")

	("arrow-line-width"	"cap"	"track"		pd::ck-int
	 "The width of lines used to draw arrows on aircraft tracks."
	 (0 . 10))

	("arrow-scale"		nil		nil	pd::ck-float
	 "The scale factor used to determine the length of the arrows.")

	("axis-color"		"tseries"	nil	nil
	 "The color to use in drawing time series axes.")

	("azimuth-interval"	"cap"	"overlay"	pd::ck-float
	 "The spacing between azimuth lines, in degrees")

	("azimuth-offset"	nil	nil		pd::ck-float
	 "The offset of the range rings from true north, in degrees.")

	("center"		nil	nil		pd::ck-float
	 "The center value used for the color coding of data and/or the
selection of contour values.  This parameter is qualified first by the
field name, then the plot representation, yielding something like
'raster-velocity-center'")

	("closed-boundary"	"cap"	"overlay"	pd::ck-bool
	 "For boundary overlays, determines whether the boundary will be 
drawn closed (first point connected to the last) or not.")

	("ta-color"		nil		nil	nil
	 "The color to use for annotation at the top of the screen.")

	("color"		nil		nil	nil
	 "The color used to draw this overlay")

	("color-mono"		"cap"		"contour"	pd::ck-bool
	 "TRUE if the contours drawn in this overlay should be monochromatic")

	("color-table"		nil		nil	nil
	 "The name of the color table to use to encode the data")

	("comment"	nil		nil	nil
	 "Just a comment -- not interpreted by anybody")

	("ct-limit"		"cap"		"contour"	pd::ck-int
	 "Used to cut down on the number of colors which appear in the 
side annotation.  A CT-LIMIT value of N means only show every Nth value."
	 ( 1 . 20 ))

	("data-available-command"	nil	nil	nil
	 "The command to run when a time is selected out of the data
available menu.")

	("data-skip"		"cap"	"track"		pd::ck-int
	 "Specifies an optional thinning of data for aircraft tracks.  If
this value is set to N, only one out of every N points will be plotted."
	 (1 . 100))

	("degrade"		"cap"	"vector"	pd::ck-int
	 "Specifies an optional thinning of vector grid data.  If degrade
is set to N, only one out of every N points (in both dimensions) will be
plotted."
	 (1 . 20))

	("desc"			nil	nil	nil
	 "A long description of a field (used to qualify the name).  This
parameter is old, and, I think, not used any longer.  Field descriptions
should instead go in .../lib/FieldDefs")

	("disable"		nil	nil		pd::ck-bool
	 "A boolean value controlling the display of this overlay.  If
DISABLE is TRUE, this overlay will not be displayed when the window is 
replotted.  If it is FALSE (default) the overlay will be visible.")

	("disabled-icon-background"	nil	nil	nil
	 "The background color to use for icons representing disabled
	  overlays.")

	("do-feet"		"skewt"	nil		pd::ck-bool
	 "True if skew-t plots should be annotated in feed; otherwise
kilometers are used.")

	("do-labels"		"cap"	"contour"	pd::ck-bool
	 "TRUE if contours should be labelled with their numeric values.")

	("every-sweep"		nil	nil		pd::ck-bool
	 "If true, and radar-space is in effect, then every sweep of
the radar will be displayed, regardless of elevation.  If false, then
only the sweep closest to the given altitude will be displayed")

	("field"	nil		nil	nil
	 "The field to be displayed in this overlay")

	("file-path"		nil	nil		nil
	 "The name of a directory used for scratch files (frame cache
files, in particular).")

	("filter-attribute"	nil	nil		nil
	 "Attribute used to test observations for inclusion in the 
data available menu.  Is also used by the raster plot module to filter out
uninteresting (i.e. non-surveillance) sweeps.")

	("flip-time"		"tseries" nil		pd::ck-bool
	 "TRUE if the time sense in time-series plots should be reversed,
so that time proceeds from right to left.")

	("frame-rate"		nil	nil		pd::ck-int
	 "The default frame rate to appear in the movie control widget."
	 (1 . 10))

	("frame-skip"		nil	nil		pd::ck-int
	 "The default frame skip (in minutes) to appear in the movie control
widget"
	 ( 1 . 600 ))

	("grid"			"cap"		"vector"	pd::ck-bool
	 "Determines whether irregular grid data (i.e. mesonetworks) should
be rendered to a regular grid before plotting.  If TRUE, the data are
gridded and arrows drawn at the grid points.  Otherwise arrows are drawn
at the station locations.")

	("tic-width"		nil		nil	pd::ck-int
	 "The width of tics drawn in cartesian grid overlays, in pixels."
	 (1 . 50))

	("icon"			nil	nil	nil
	 "The name of the icon to use to represent this overlay")

	("icon-age-background"		nil	nil	nil
	 "The background color to use in icons representing old data")

	("icon-age-foreground"		nil	nil	nil
	 "The foreground color to use in icons representing old data")

	("icon-background"		nil	nil	nil
	 "The background color to use for normal icons")

	("icon-left-menu" 	nil	nil	nil
	 "The menu to be displayed when the left button is pressed in
the icon for this overlay")

	("icon-middle-menu" 	nil	nil	nil
	 "The menu to be displayed when the middle button is pressed in
the icon for this overlay")

	("icon-right-menu" 	nil	nil	nil
	 "The menu to be displayed when the right button is pressed in
the icon for this overlay")

	("highlight"		"cap"	"raster"	pd::ck-float
	 "The data value which should be drawn in the highlight color.")

	("highlight-color"	"cap"	"raster"	nil
	 "The color to use in drawing the highlighted data value.")

	("highlight-range"	"cap"	"raster"	nil
	 "The range of data (around the highlight value) which should be
drawn in the highlight color.")

	("label"		"cap"	"overlay"	nil
	 "Determines whether labels are drawn on certain overlay plots 
(such as locations and boundaries) or not.")

	("label-blanking"	"cap"	"contour"	pd::ck-bool
	 "TRUE if the area underneath contour labels should be blanked
out to improve their readability (at the cost of losing the underlying
data.")

	("label-size"		nil		nil	pd::ck-float
	 "The size of labelling, as a portion of the size of the window
as a whole.  Typical values are 0.02 or so...")

	("lat-lon"		"cap"	"overlay"	pd::ck-bool
	 "TRUE if grid overlays should be done as a latitude-longitude
grid; if false, an X/Y (kilometer) grid is drawn instead")

	("limit-proc"		nil		nil	nil
	 "The name of a procedure to run when 'adjust limits' is selected
by the user")

	("line-width"		nil		nil	pd::ck-int
	 "The width of lines used to draw this overlay"
	 (0 . 20))

	("location"		nil		nil	pd::ck-loc
	 "The location of some platform, used to qualify the parameter.")

	("max-frames"		nil		nil	pd::ck-int
	 "The maximum number of frames which can be stored in the frame
cache file."
	 (0 . 100))

	("movie-end-time"	nil	nil	nil
	 "The default end time to go into the movie control widget")

	("movie-minutes"	nil		nil	pd::ck-int
	 "The default time period for the movie control widget"
	 (1 . 9999))

	("movie-pregenerate"	nil		nil	nil
	 "A comma-separated list of fields.  When a movie is generated,
frames will be made with each of the listed fields substituted into the 
base overlay.")

	("nsteps"		nil		nil	pd::ck-int
	 "The number of color steps to use in raster plots.  Qualified by
	  field name.")

	("origin-alt"		nil		nil	pd::ck-float
	 "The altitude of the origin.")

	("origin-lat"		nil		nil	pd::ck-float
	 "The latitude of the origin.")

	("origin-lon"		nil		nil	pd::ck-float
	 "The longitude of the origin.")

	("out-of-range-color"	"cap"	"track"		nil
	 "The color to use for the track when the data used for color coding
is out of the range allowed.")

	("pd-name"		nil	nil	nil
	 "The name of this plot description.  This parameter MUST appear
in the global component.")

	("platform"	nil		nil	nil
	 "The platform to be displayed in this overlay")

	("plot-mode"	nil		nil		pd::ck-enum
	 "The mode in which we are plotting.  Needs to be either HISTORY
or REAL-TIME"
	 ("history" "real-time"))

	("plot-time"		nil	nil	nil
	 "The time that this plot takes place")

	("plot-type"		nil		nil	pd::ck-enum
	 "The type of this plot.  Valid types are CAP (constant altitude
plot), skewt, xsect, tseries, and xygraph"
	 ("cap" "skewt" "xsect" "tseries" "xygraph"))

	("pos-origin"		nil		nil	nil
	 "A comma-separated list of possible origins for the position widget")

	("position-icon"	"cap"	"track"		nil
	 "The name of a file containing the icon to display at the platform
position on a track.")

	("post-proc-mode"	nil		nil	pd::ck-bool
	 "TRUE if the system is running in the post processing mode.  This
	  parameter has a number of effects on how the system views times.")

	("pp-dm-time-adjust"		nil	nil	pd::ck-interval
	 "An interval specifying what the skip time should be in the 
	  display manager time controller.")
	
	("quad-color"		"cap"	"vector"	nil
	 "The color to use when annotating quadrants around station vectors.")

	("quadrants"		"cap"	"vector"	nil
	 "A comma-separated list of fields to display in the four quadrants
around the arrow in station plots.")

	("radar-space"		"cap"	nil	pd::ck-bool
	 "Controls whether the window operates in radar space.  In the radar
space mode, altitudes are interpreted in degrees, and image selection 
criteria are a bit different.")

	("range"		"cap"	"overlay"	pd::ck-float
	 "The range to which the azimuth limits overlay is drawn from
the origin.")

	("range-min"		nil	nil	pd::ck-float
	 "The minimum acceptable data value before the data will be treated
as a bad value.  This parameter is qualified by the field name, and does not
make much sense without that qualification.")

	("range-max"		nil	nil	pd::ck-float
	 "The maximum acceptable data value before the data will be treated
as a bad value.  This parameter is qualified by the field name, and does not
make much sense without that qualification.")

	("representation" nil		nil	nil
	 "The representation of this overlay.  Possibilities will be
listed here later on.")

	("ring-interval"	"cap"	"overlay"	pd::ck-float
	 "The interval between rings in the range rings display, in km.")

	("sa-scale"		nil	nil		pd::ck-float
	 "The size of side annotation, as a portion of the size of the window
as a whole.")

	("show-position"	"cap"	"track"		pd::ck-bool
	 "TRUE if the position of the aircraft is to be shown on the track.")

	("solid"		nil	nil		pd::ck-bool
	 "If true, cartesian grids are drawn as solid lines; otherwise they
are drawn as tics at the grid points.")

	("step"			nil	nil		pd::ck-float
	 "The step value used for the color coding of data and/or the
selection of contour values.  This parameter is qualified first by the
field name, then the plot representation, yielding something like
'raster-velocity-step'")

	("ta-color-match"	nil	nil	pd::ck-bool
	 "TRUE if the color of the annotation at the top of the screen
should match the color of the data, if possible.")

	("time-frames"	nil		nil	nil
	 "The number of pixmap frames to be kept in memory")

	("time-period"		"cap"	"track"		pd::ck-interval
	 "The length of an aircraft track, as a time interval.")

	("trigger"		nil	nil		nil
	 "The condition which causes a replot.  In the GLOBAL component, the
trigger causes the entire window to be replotted; otherwise just the given
component will be updated.  Triggers may be either the name of a platform,
or a time interval.")

	("trigger-global"	nil	nil		pd::ck-bool
	 "Determines whether a trigger in this component is treated as
a global trigger.")

	("x-max"		nil	nil		pd::ck-float
	 "The northernmost extent of the window relative to the origin, in km")

	("x-min"		nil	nil		pd::ck-float
	 "The southernmost extent of the window from the origin, in km")

	("x-spacing"		"cap"	"overlay"	pd::ck-float
	 "The spacing between vertical grid lines, in degrees longitude if
lat-lon is TRUE; kilometers otherwise.")

	("xorvalue"		nil	nil		pd::ck-int
	 "A bitwise value used for exclusive oring against the display when
rubber-banding is being done.  Best not to mess with it.")

	("y-max"		nil	nil		pd::ck-float
	 "The northernmost extent of the window relative to the origin, in km")

	("y-min"		nil	nil		pd::ck-float
	 "The southernmost extent of the window from the origin, in km")

	("y-spacing"		"cap"	"overlay"	pd::ck-float
	 "The spacing between horizontal grid lines, in degrees latitude if
lat-lon is TRUE; kilometers otherwise.")
))




;
; Look up a rule.
;
(defun pd::find-rule (param &optional nonly) "Look up a rule with this param"
    (pd::rec-find-rule param pd::ParamRules nonly)
)

(defun pd::rec-find-rule (param rules nonly) "Really look for the rule."
    (cond
    	((null rules) nil)
	((and (string-match (concat (pd::r-param (car rules)) "$") param)
	      (or nonly (null (pd::r-ptype (car rules)))
	      	  (string-equal (pd::r-ptype (car rules)) pd::plot-type))
	      (or nonly (null (pd::r-rep (car rules)))
	          (string-equal pd::component "global") ; XXX
	          (string-equal (pd::r-rep (car rules))
		  	(pd::retrieve pd::component "representation"))))
	    (car rules))
	(t (pd::rec-find-rule param (cdr rules) nonly))
    )
)


(defun pd::find-rule2 (param) "Another implementation"
    (let ((rule pd::ParamRules) (ret nil))
    	(while (and rule (not ret))
	    (if (pd::rule-match param (car rule))
		(setq ret (car rule))
		(setq rule (cdr rule))
	    )
	)
	ret
    )
)



(defun pd::rule-match (param rule) "Does this param match this rule?"
    (and (string-match (concat (pd::r-param rule) "$") param)
	(or (null (pd::r-ptype rule))
	     (string-equal (pd::r-ptype rule) pd::plot-type))
	(or (null (pd::r-rep rule))
	     (string-equal pd::component "global") ; XXX
	     (string-equal (pd::r-rep rule)
		(pd::retrieve pd::component "representation"))))
)



;;
;; Attempt to deal with rules through property lists.
;;
(defun pd::prop-rules (rules) "Put the rules into a property list."
    (let ((rule rules))
        (while rule
		(put (intern (pd::r-param (car rule))) 'rule (car rule))
    		(setq rule (cdr rule))
	)
    )
)

;
; Actually store it all now.
;
(pd::prop-rules pd::ParamRules)

;
; Look up a rule via the property list.
;
(defun pd::prop-find-rule (param)
    (cond
    	((get (intern param) 'rule))	; Found it
	((string-match "-" param)
	    (pd::prop-find-rule (substring param (+ (match-beginning 0) 1))))
	(t nil)
    )
)






(defun pd::rule-check (stuff rule) "Apply the rule"
    (let ((cfun (pd::r-check rule)))
        (if cfun
	    (funcall cfun (downcase stuff) (pd::r-cdata rule)))
    )
)



;;-----------------------------------------------------------
;; Check functions.
;;

(defun pd::ck-float (stuff junk) "Check to see that this is a float"
    (if (not (string-match "^-?[0-9]+\.?[0-9]*$" stuff))
    	(pd::gripe "This parameter must be a floating-point number"))
)


(defun pd::ck-bool (stuff junk) "Check booleans"
	(or
	    (string-equal stuff "true")
	    (string-equal stuff "false")
	    (pd::gripe "This parameter must be TRUE or FALSE"))
)


(defun pd::ck-enum (stuff enum) "See that stuff is one of the things in enum"
    (cond
        ((null enum) (pd::gripe "Invalid value for this parameter"))
	((string-equal stuff (car enum)) t)
	(t (pd::ck-enum stuff (cdr enum)))
    )
)


;
; Check an integer.  Also does range checking if a range is supplied.
;
(defun pd::ck-int (stuff range) "Check an integer"
    (or (string-match "^-?[0-9]+$" stuff)
    	(pd::gripe "This parameter must be an integer"))
    (if range
	(let ((ival (string-to-int stuff)))
	    (if (or (< ival (car range)) (> ival (cdr range)))
	    	(pd::gripe
		(format "Value out of range %d to %d" (car range) (cdr range)))
	     )
	)
    )
)

;
; Check trigger intervals.
;
(defun pd::ck-interval (stuff junk) "check an interval"
    (or (string-match "^[0-9]+[hmsd]?$" stuff) (pd::gripe "Bad time interval"))
)



(defun pd::ck-loc (stuff junk) "Check to see that this is a location"
    (if (not (string-match
	     "^-?[0-9]+\.?[0-9]*[ \t]*-?[0-9]+\.?[0-9]*[ \t]*[0-9]+\.?[0-9]*$"
	stuff))
    	(pd::gripe "This parameter must be a location."))
)

;;------------------------------------------------------------
;; Markup code below here.
;;

(defvar pd::comp-style (make-style) "The style to display components")
(set-style-foreground pd::comp-style "blue3")

(defvar pd::param-style (make-style) "Style for ordinary parameters")

(defvar pd::unk-param-style (make-style) "Style for unknown parameters")
(set-style-foreground pd::unk-param-style "red")

(defvar pd::gripe-style (make-style) "Style for complaints")
(set-style-foreground pd::gripe-style "red")
(set-style-underline pd::gripe-style "black")

;
; A function to return everything to normal.
;
(defun pd::reset-zones () "Reset the zones"
	(epoch::clear-zones)
)


;
; Mark up a line.
;
(defun pd::mark-line (where style) "Mark this line in the given style"
    (save-excursion
    	(goto-char where)
	(beginning-of-line)
	(let ((begin (point)))
	    (end-of-line)
	    (add-zone begin (point) style)
	)
    )
)


;;---------------------------------
;; Below here is the code for the pd monitor hookin capability.
;;
(defvar pd::mon-buffer nil "The monitor buffer")
(defvar pd::control-buffer nil "The control buffer")
(defvar pd::mon-screen nil "The monitor screen")
(defvar pd::mon-state 'idle "The monitor state")
(defvar pd::mon-process nil "The monitor process")
(defvar pd::mon-expect 0 "Number of chars expected")
(defvar pd::mon-position 0 "Where we were before new PD arrived")
(defvar pd::help-screen nil "The help screen")
(defvar pd::help-buffer nil "The help buffer")



(defun pd::monitor (process) "Hook into a running graphics process"
    (interactive "sMonitor which process: ")
;
; Create the buffer and screen, if necessary.
;
    (setq pd::mon-buffer (get-buffer-create (concat "*pdmon-" process "*")))
    (setq pd::control-buffer (get-buffer-create
    		(concat "*pdmon-" process "-control*")))
    (setq pd::mon-screen (or
	    (car (epoch::screens-of-buffer pd::mon-buffer))
	    (create-screen pd::mon-buffer nil)
	    		(list '(title . "Plot description monitor")))
    )
;
; Get the screen up on the display and put the buffer there.
;
    (mapraised-screen pd::mon-screen)
    (select-screen pd::mon-screen)
    (switch-to-buffer pd::mon-buffer)
;
; Make the control panel.
;
    (pd::make-control)
;
; Create our monitor process.
;
    (setq pd::mon-process
    	(start-process "pdmon" pd::mon-buffer "pdmon" process))
    (set-process-filter pd::mon-process 'pd::mon-filter)
    (set-process-sentinel pd::mon-process 'pd::proc-sentinel)
    (setq pd::mon-state 'idle)
)





(defun pd::mon-filter (process stuff) "The process filter"
    	(if (eq pd::mon-state 'idle)
		(pd::new-pd stuff)
		(pd::pd-continue stuff)
	)
)




(defun pd::new-pd (stuff) "Initiate a new plot description"
    (save-excursion
;
; Clear out the buffer.
;
	(setq pd::mon-state 'reading)
	(set-buffer pd::mon-buffer)
	(setq pd::mon-position (point))
	(pd::reset-zones)
	(erase-buffer)
;
; See what is coming.
;
	(setq pd::mon-expect (string-to-int stuff))
	(message "Expecting PD of len %d" pd::mon-expect)
;
; See if part of the new PD is in this string.
;
	(let ((mdata (match-data)))
	    (if (and (string-match "\n" stuff)
	    		(> (length stuff) (match-end 0)))
	        (insert (substring stuff (match-end 0))))
	    (store-match-data mdata)
	)
    )
)



;
; Deal with another chunk of the plot description, and see if we are done.
;
(defun pd::pd-continue (stuff) "Continue with a PD"
    (set-buffer pd::mon-buffer)
    (goto-char (point-max))
    (insert stuff)
    (message "Now have %d bytes" (buffer-size))
    (if (>= (buffer-size) pd::mon-expect)
     	(progn
	    (message "PD received.  Checking...")
	    (setq pd::mon-state 'idle)
	    (goto-char pd::mon-position)
	    (sit-for 0)
	    (pd::check)
	)
    )
)



;
; When we're finished.
;
(defun pd::done () "All done with the monitor function."
    (interactive)
    (process-send-eof "pdmon")
    (delete-screen pd::mon-screen)
    (if pd::help-screen (delete-screen pd::help-screen))
    (kill-buffer pd::mon-buffer)
    (kill-buffer pd::control-buffer)
    (setq pd::help-screen nil)
)


;
; The sentinel process. We just assume it died.
;
(defun pd::proc-sentinel (process event) "The process sentinel"
    (message "PD monitor process died")
    (delete-screen pd::mon-screen)
    (if pd::help-screen (delete-screen pd::help-screen))
    (setq pd::help-screen nil)
)


;
; The procedure to send a plot description back out to the graphics
; process.
;
(defun pd::send () "Send out the plot description"
    (interactive)
    (save-excursion
    	(set-buffer pd::mon-buffer)
	(pd::check)
	(process-send-string pd::mon-process
			(concat (int-to-string (buffer-size)) "\n"))
	(process-send-region pd::mon-process (point-min) (point-max))
    )
)


;
; Help.
;
(defun pd::help () "Get help on this line."
     (interactive)
;
; If we do not yet have a help screen, make it.
;
    (if (or (null pd::help-screen) (null (screen-information pd::help-screen)))
    	(progn
	    (setq pd::help-buffer (get-buffer-create "*pdmon-help*"))
	    (setq pd::help-screen (or
		(car (epoch::screens-of-buffer pd::help-buffer))
		(create-screen pd::help-buffer
	    		(list '(title . "Plot description help")
				'(geometry . "80x16")))
    	    ))
	)
    )
;
; Get the screen up on the display and put the buffer there.
;
    (mapraised-screen pd::help-screen)
;    (select-screen pd::help-screen)
;    (switch-to-buffer pd::help-buffer)
    (save-excursion
    	(switch-to-buffer pd::help-buffer)
	(erase-buffer)
    )
;
; Now deal with the help info.
;
   (save-excursion
	(beginning-of-line)
	(let ((begin (point)))
		(end-of-line)
		(pd::present-help (buffer-substring begin (point)))
	)
    )
)





;
; Actually present some help information.
;
(defun pd::present-help (line) "Present help on this line"
    (save-excursion
    (set-buffer pd::help-buffer)
    (cond
    	((eq (string-to-char line) ?!)
	    (insert "Lines beginning with ! are comments"))
	((eq (string-to-char line) ?\t)
	    (insert (pd::pname line) "\n\n")
	    (let ((rule (pd::prop-find-rule (pd::pname line))))
	        (insert (if rule (pd::r-descr rule) 
			"I do not know about this parameter, sorry."))
	    )
	)
	(t (insert (concat "This line starts a new component named " line)))
    ))
)


;
; Control panel styles.
;
(defvar pd::cb-normal-style (make-style) "Normal button state")
(set-style-foreground pd::cb-normal-style "black")
(set-style-background pd::cb-normal-style "gray80")

(defvar pd::cb-press-style (make-style) "Pressed button state")
(set-style-background pd::cb-press-style "black")
(set-style-foreground pd::cb-press-style "gray80")

;
; The buttons.
;
; ( text action )
;
(setq pd::cb-actions '(
	( "send PD"	pd::send )
	( "check PD"	pd::check )
	( "help"	pd::help )
	( "quit"	pd::done )
))

;
; The mouse map which controls our actions.
;
(defvar pd::cp-mmap (create-mouse-map) "The control panel mouse map")
(define-mouse pd::cp-mmap mouse-left mouse-down 'pd::cp-mouse)
(define-mouse pd::cp-mmap mouse-middle mouse-down 'pd::cp-mouse)
(define-mouse pd::cp-mmap mouse-right mouse-down 'pd::cp-mouse)

;
; The control panel.
;
(defun pd::make-control () "Make the control panel"
;
; Put the control window at the top of the screen.
;
    (let ((window-min-height 3))
        (split-window nil 3)
    )
    (switch-to-buffer pd::control-buffer)
    (use-local-mouse-map pd::cp-mmap)
;
; Fill in some text.
;
    (erase-buffer)
    (insert "PD monitor control panel\n")
;
; Put in the buttons.
;
    (let ((bdef pd::cb-actions) ppoint)
        (while bdef
	    (setq ppoint (point))
	    (insert "  " (caar bdef) "  ")
	    (add-zone (+ ppoint 1) (- (point) 1) pd::cb-normal-style
			(nth 1 (car bdef)))
	    (setq bdef (cdr bdef))
	)
    )
    (select-window (next-window))
)




;
; The mouse event handler.
;
(defun pd::cp-mouse (mdata) "The mouse handler"
    (let ((zone (zone-at (car mdata) pd::control-buffer)))
        (if zone
	    (progn
		(set-zone-style zone pd::cb-press-style)
		(sit-for 0)
		(funcall (zone-data zone))
		(set-zone-style zone pd::cb-normal-style)
	    )
	    (message "no button here")
	)
    )
)
