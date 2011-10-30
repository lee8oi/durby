################################################################################
# Original Webby Author information.                                
# Copyleft (C) 2009-2011, speechles                             
# imspeechless@gmail.com                                        
# October 5th, 2011                                             
################################################################################
##################################################################################
# Copyright ©2011 lee8oi@gmail.com
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
# GNU General Public License for more details.
# http://www.gnu.org/licenses/
#
# Durby v0.1
# by: <lee8oiAtgmail><lee8oiOnfreenode>
#
# This is an unofficial version of 'webby'.
#
# This script will display titles and other relevant information
# from links given in irc channels. Can also be used to test    
# and contstruct regular expression in channel.                 
#
# NOTE: Theres alot of languages and characters out there. To make best use of
# this script all eggdrop bots *version 1.6.20 and Under will need to be patched
# to utf-8 encoding as instructed on the following link: http://eggwiki.org/Utf-8
# Non-patched bots may still get some use of this script but some characters from
# certain languages may not show correctly.
#
# * eggdrop version 1.8+ is rumored to have encoding issues corrected. Eliminating
# the need to patch the bot.
#
# Usage:                                                        
#   .chanset #channel +webby                                    
#   !webby website.here.com [--html] [--header] [--xheader]     
#       [--post] [--override] [--nostrip] [--swap]              
#       [--regexp regexp-here--]                                
#
# Durby Changelog:
# v0.1
#   .1.Cleaned up code indenting. Took care of some escapes needed in broken
#   strings. Fixed a couple minor brace issues.
#   .2.Fixed utf-8 issues for utf-8 patched eggdrop bots. Tested in 1.6.20.
#   script is now primarily for patched bots.
#   .3.Added url grabber. Script can now be set to watch for urls in channels 
#   with the 'webby' flag set.
#   .4.Added tls require and https fix.(noticed that when I disabled the ONLY other
#   script that used tls; Webby lost https url support).
#   .5.Cleaned up some extra debugging code that I left behind. This fixes some
#   minor issues with backslashes appearing in the html on utf-8 encoded pages.
#   .6.Fixed bug that caused webby to run twice when using the !webby command.
#   .7.Pattern based url ignore system implemented. Patterns and enable/disable can be
#   found in config section.
#
################################################################################
# Original Webby Author information.                                
# Copyleft (C) 2009-2011, speechles                             
# imspeechless@gmail.com                                        
# October 5th, 2011                                             
################################################################################
# ---> Start of config; setting begins

# do you want to display header attributes always?
# --- [ 0 no / 1 yes ]
variable webbyheader 0

# do you want to display x-header attributes always?
# --- [ 0 no / 1 yes ]
variable webbyXheader 0

# do you want to display html attributes always?
# --- [ 0 no / 1 yes ]
variable webbydoc 0

# if using the regexp (regular expression) engine do
# you want to still show title and description of webpage?
# --- [ 0 no / 1 yes ]
variable webbyRegShow 0

# max length of each line of your regexp capture?
# --- [ integer ]
variable webbySplit 403

# how many regexp captures is the maximum to issue?
# presently going above 9, will only result in 9...
# --- [ 1 - 9 ]
variable webbyMaxRegexp 9

# which method should be used when shortening the url?
# (0-3) will only use the one you've chosen.
# (4-5) will use them all.
# 0 --> http://tinyurl.com
# 1 --> http://u.nu
# 2 --> http://is.gd
# 3 --> http://cli.gs
# 4 --> randomly select one of the four above ( 2,0,0,3,1..etc )
# 5 --> cycle through the four above ( 0,1,2,3,0,1..etc )
# ---  [ 0 - 5 ]
variable webbyShortType 5

# regexp capture limit
# this is how wide each regexp capture can be, prior to it
# being cleaned up for html elements. this can take a very
# long time to cleanup if the entire html was taken. so this
# variable is there to protect your bot lagging forever and
# people giving replies with tons of html to lag it.
# --- [ integer ]
variable regexp_limit 3000

# how should we treat encodings?
# 0 - do nothing, use eggdrops internal encoding whatever that may be.
# 1 - use the encoding the website is telling us to use.
#     This is the option everyone should use primarily.
# 2 - Force a static encoding for everything. If you use option 2,
#     please specify the encoding in the setting below this one.
# --- [ 0 off-internal / 1 automatic / 2 forced ]
variable webbyDynEnc 1

# option 2, force encoding
# if you've chosen this above then you must define what
# encoding we are going to force for all queries...
# --- [ encoding ]
variable webbyEnc "iso8859-1"

# fix Http-Package conflicts?
# this will disregard http-packages detected charset if it appears
# different from the character set detected within the meta's of
# the html and vice versa, you can change this behavior
# on-the-fly using the --swap parameter.
# --- [ 0 no / 1 use http-package / 2 use html meta's ]
variable webbyFixDetection 2

# report Http-package conflicts?
# this will display Http-package conflict errors to the
# channel, and will show corrections made to any encodings.
# --- [ 0 never / 1 when the conflict happens / 2 always ]
variable webbyShowMisdetection 0

# grab urls from channel messages?
# this will set Durby to watch for urls in channel and
# post url information when one is found.
variable webbyWatchForUrls 1

# ignore urls matching ignore pattern?
# enabling this option will set webby to ignore urls that match
# any pattern in the patterns list.
variable webbyPatternIgnore 1

# What patterns should be ignored?
# patterns can be any valid 'string match' pattern. Urls matching
# any of these patterns will be ignored if webbyPatternIgnore option is
# enabled.
variable patterns {
  *.jpg
  *.png
  *.pdf
  *.gif
  *.mov
  *.zip
  *.exe
  *.tar
  *.gz
  *.rar
  *.bmp
  #*porn.com*
}
################################################################################
# END CONFIGURATION - EXPERTS ONLY BELOW!
################################################################################
# todo: fix gzip handling of utf-8 encoded urls
#   2. Allow title only or less verbose output options
#   3. Fix system to check header for content in case of image links/etc.
#   3. Fix character garble in descriptions.
#<speechles> you call validate instead of full http::geturl
#<speechles> then if passes, attempt geturl again without the -validate 1 option
#<speechles> depending on mime type given in the meta array
#speechles> thats his utf-8 patched bot
#<speechles> webby keeps 4 charsets
#<speechles> state(charset) http package gives it
#<speechles> it cleans that up, to char
#<speechles> then char2 metas give it, it cleans that up to char3
#<speechles> when char and char3 differ
#<speechles> you should encoding convertfrom char3
#<speechles> otherwise wont show up right, this is known as the conflict
#<speechles> so the easy way to do this
#<speechles> is always... if char3 is there, encoding convertfrom it
#<speechles> otherwise encoding convertfrom char2
#<speechles> or convertto char2 rather
#<speechles> the problem is http package
#<speechles> when it gets the encoding right
#<speechles> the socket fconfigured correct
#<speechles> the encoding matches, what it really is
#<speechles> you can encoding convertto that encoding just fine
#<speechles> but.. if it was fconfigured incorrectly
#<speechles> with teh wrong encoding say
#<speechles> http package thinks its iso8859-1
#<speechles> when really, its cp1251
#<speechles> you have to convertfrom cp1251
#<speechles> rather than convertto
#<speechles> because the socket got it wrong... so you have to undo its damage
#<speechles> after u convertfrom cp1251
#<speechles> you would convertto utf-8 if the [system encoding] is not utf-8
#<speechles> this fixes it for unpatched bot
package require http
package require tls
::http::register https 443 [list ::tls::socket -require 0 -request 1]
if {([lsearch [info commands] zlib] == -1) && ([catch {package require zlib} error] !=0)} {
  if {([catch {package require Trf} error] == 0) || ([lsearch [info commands] zip] != -1)} {
    putlog "Webby: Found trf package. Fast lane activated!"
    set webbyTrf 1
  } else {
    putlog "Webby: Cannot find zlib or trf package! Gzipped url queries will not be used. Enjoy the slow lane! :P"
    set webbyNoGzip 1
  }
} else {
  putlog "Webby: Found zlib package. Fast lane activated!"
}
set weburlwatch(titlegrab) 0
set weburlwatch(pubmflags) "-|-"
set weburlwatch(delay) 1
set weburlwatch(last) 111
set weburlwatch(length) 5
set weburlwatch(watch) 1
setudef flag webby
bind pub - !webby webby
bind pubm weburlwatch(pubmflags) {*://*} weburlwatch

proc weburlwatch {nick host user chan text} {
  # watch for web urls in channel
  variable weburlwatch
  if {([channel get $chan webby]) && ([expr {[unixtime] - $weburlwatch(delay)}] > $weburlwatch(last))\
    && ($::webbyWatchForUrls > 0)} {
    foreach word [split $text] {
      if {($word == "!webby")} {
          return 0
      } else {
        if {[string length $word] >= $weburlwatch(length) && \
        [regexp {^(f|ht)tp(s|)://} $word] && ![regexp {://([^/:]*:([^/]*@|\d+(/|$))|.*/\.)} $word]} {
          #check ignore list
          if {$::webbyPatternIgnore > 0} {
            foreach pattern $::patterns {
              if {[string match -nocase $pattern $word]} {
                return 0
              }
            }
          }
          set weburlwatch(last) [unixtime]
          set weburlwatch(titlegrab) 1
          set urtitle [webby $nick $host $user $chan $word]
          break
        }
      }
    }
  }
  # change to return 0 if you want the pubm trigger logged additionally..
  return 1
}
proc webby {nick uhost handle chan site} {
  if {![channel get $chan webby]} { return }
  if {[regsub -nocase -all -- {--header} $site "" site]} { set w1 0 }
  if {[regsub -nocase -all -- {--validate} $site "" site]} { set w1 0 ; set w2 0 ; set w3 0 ; set w10 0 }
  if {[regsub -nocase -all -- {--xheader} $site "" site]} { set w2 0 }
  if {[regsub -nocase -all -- {--html} $site "" site]} { set w3 0 }
  if {[regsub -nocase -all -- {--post} $site "" site]} { set w4 0 }
  if {[regsub -nocase -all -- {--override} $site "" site]} { set w6 0 }
  if {[regsub -nocase -all -- {--nostrip} $site "" site]} { set w7 0 }
  if {[regsub -nocase -all -- {--swap} $site "" site]} { set w8 0 }
  if {[regsub -nocase -all -- {--gz} $site "" site]} { set w9 0 }
  if {[regexp -nocase -- {--regexp (.*?)--} $site - reggy]} {
    if {[catch {set varnum [lindex [regexp -about -- $reggy] 0]} error]} {
      putserv "privmsg $chan :\002regexp\002 [set error]"
      return 0
    } elseif {$varnum > $::webbyMaxRegexp} {
      putserv "privmsg $chan :\002regexp\002 too many captures ($varnum), reducing to ($::webbyMaxRegexp)"
      set varnum $::webbyMaxRegexp
    }
    set w5 0
    regsub -nocase -- {--regexp .*?--} $site "" site
  }
  #check ignore list
  if {$::webbyPatternIgnore > 0} {
    foreach pattern $::patterns {
      if {[string match -nocase $pattern $site]} {
        return 0
      }
    }
  }
  if {[string match "*://*" $site]} { set site [join [lrange [split $site "/"] 2 end] "/"] }
  if {[string equal "-" [string index [set site [string map {"  " " "} [string trim $site]]] 0]]} {
    putserv "privmsg $chan :\002webby\002: you've used an improper flag which may exploit http-package!"
    return
  }
  foreach e [split $site "."] { lappend newsite [idna::domain_toascii $e] }
  set fullquery "http://[join $newsite "."]"
  set ua "Mozilla/5.0 (Windows; U; Windows NT 5.1; en-US; rv:1.9.0.5) Gecko/2008120122 Firefox/3.0.5"
  set http [::http::config -useragent $ua]
  if {[info exists w4]} {
    #w4 is post
    putserv "privmsg $chan :Deconstructing post query..."
    if {![regexp -- {\|} $fullquery] && ![regexp -- {\?} $fullquery]} {
      putserv "privmsg $chan :\002webby\002: $fullquery does not appear to be a post query, using simple query..."
      unset w4
    } else {
      if {[regexp -- {\|} $fullquery]} {
        set url [lindex [split $fullquery "|"] 0]
        set query [lindex [split $fullquery "|"] 1]
        if {[regexp -- {\?} $query]} {
          set suburl "/[lindex [split $query "?"] 0]?"
          set query [lindex [split $query "?"] 1]
        } else { set suburl "" }
        putserv "privmsg $chan :\002webby\002: detected \002url\002 $url \002query\002 $query"
      } else {
        set suburl ""
        set url [lindex [split $fullquery "?"] 0]
        set query [lindex [split $fullquery "?"] 1]
        putserv "privmsg $chan :\002webby\002: detected \002url\002 $url \002query\002 $query"
      }
      set post [list]
      foreach {entry} [split $query "&"] {
        if {![regexp {\=} $entry]} {
          putserv "privmsg $chan :\002webby\002: broken post structure ($entry). Is not post query, using simple query..."
          unset w4
          break?
        } else {
          set name [lindex [split $entry "="] 0]
          set value [lindex [split $entry "="] 1]
          set post [concat $post "$name=$value"]
        }
      }
    }
    if {[info exists w4]} { putserv "privmsg $chan :Posting..." } ;# W4 IS --POST
  }
  set value ""
  if {[info exists w9]} {
    # w9 is --gz
    set gzp "Accept-Encoding gzip,deflate"
  } else {
    # no --gz
    set gzp ""
  }
  if {[info exists w4]} {
    # w4 is --post
    if {[info exists w10]} {
      # w10 is --validate
      catch {set http [::http::geturl "$url$suburl" -query "[join $post "&"]" -headers $gzp -validate 1 -timeout 10000]} error
    } else {
      # no --validate
      catch {set http [::http::geturl "$url$suburl" -query "[join $post "&"]" -headers $gzp -timeout 10000]} error
    }
  } else {
    # no --post
    if {[info exists w10]} {
      # w10 is --validate
      catch {set http [::http::geturl "$fullquery" -headers $gzp -validate 1 -timeout 10000]} error
    } else {
      # no --validate
      catch {set http [::http::geturl "$fullquery" -headers $gzp -timeout 10000]} error
    }
    set url $fullquery ; set query ""
  }
  if {![string match -nocase "::http::*" $error]} {
    set strtotitle [string totitle [string map {"\n" " | "} $error]] 
    putserv "privmsg $chan :\002webby\002: $strtotitle \( $fullquery \)"
    return 0
  }
  # check http status
  if {![string equal -nocase [::http::status $http] "ok"]} {
    putserv "privmsg $chan :\002webby\002: [string totitle [::http::status $http]] \( $fullquery \)"
    return 0
  }
  upvar #0 $http state
  if {![info exists state(meta)]} { putserv "privmsg $chan :\002webby\002: unsupported URL error \( $fullquery \)" ; return 0 }
  set redir [::http::ncode $http]
  # iterate through the meta array
  foreach {name value} $state(meta) {
    # do we have cookies?                                                                                                                                                                             
    if {[string equal -nocase $name "Set-Cookie"]} {
      # yes, add them to cookie list                                                                                                                                                                          
      lappend webbyCookies [lindex [split $value {;}] 0]                                                                                                                                                             
    }                                                                                                                                                                                                             
  }
  if {[info exists webbyCookies] && [llength $webbyCookies]} {
    set cookies "[join $webbyCookies {;}]"
  } else {
    set cookies ""
  }
  # REDIRECT
  set r 0
  while {[string match "*${redir}*" "307|303|302|301" ]} {
    # redirect code found.
    foreach {name value} $state(meta) {
      # loop through meta info
      if {[regexp -nocase ^location$ $name]} {
        # meta location found
        if {![string match "http*" $value]} {
          # location is not http* url
          if {![string match "/" [string index $value 0]]} {
            # first char is not a '/' 
            set value "[join [lrange [split $url "/"] 0 2] "/"]/$value"
          } else {
            set value "[join [lrange [split $url "/"] 0 2] "/"]$value"
          }
        }
        if {[string match [string map {" " "%20"} $value] $url]} {
          if {![info exists poison]} {
            set poison 1 
          } else {
            incr poison
            if {$poison > 2} {
              putserv "privmsg $chan :\002webby\002: redirect error (self to self)\($url\) - ($cookies) are no help..."
              return
            }
          }
        }
        set mapvar [list " " "%20"] 
        set strtrimvar "Referer $url $gzp" 
        if {[info exists w10]} {
          # w10 is --validate
          if {[string length $cookies]} {
            catch {set http [::http::geturl "[string map $mapvar $value]" -headers "[string trim $strtrimvar] Cookie $cookies" -validate 1 -timeout [expr {1000 * 10}]]} error
          } else {
            catch {set http [::http::geturl "[string map $mapvar $value]" -headers "[string trim $strtrimvar]" -validate 1 -timeout [expr {1000 * 10}]]} error
          }
        } else {
          # no --validate
          if {[string length $cookies]} {
            catch {set http [::http::geturl "[string map $mapvar $value]" -headers "[string trim $strtrimvar] Cookie $cookies" -timeout [expr {1000 * 10}]]} error
          } else {
            catch {set http [::http::geturl "[string map $mapvar $value]" -headers "[string trim $strtrimvar]" -timeout [expr {1000 * 10}]]} error
          }
        }
        if {![string match -nocase "::http::*" $error]} {
          putserv "privmsg $chan :\002webby\002: [string totitle $error] \( $value \)"
          return 0
        }
        if {![string equal -nocase [::http::status $http] "ok"]} {
          putserv "privmsg $chan :\002webby\002: [string totitle [::http::status $http]] \( $value \)"
          return 0
        }
        set redir [::http::ncode $http]
        set url [string map {" " "%20"} $value]
        upvar #0 $http state
        if {[incr r] > 10} { putserv "privmsg $chan :\002webby\002: redirect error (>10 too deep) \( $url \)" ; return }
        # iterate through the meta array
        #if {![string length cookies]} {
          foreach {name value} $state(meta) {
            # do we have cookies?                                                                                                                                                                             
            if {[string equal -nocase $name "Set-Cookie"]} {
              # yes, add them to cookie list                                                                                                                                                                        
              lappend webbyCookies [lindex [split $value {;}] 0]         
            }
          }                                                                                                                                                    
          if {[info exists webbyCookies] && [llength $webbyCookies]} {
            set cookies "[join $webbyCookies {;}]"
          } else {
            set cookies ""
          }
        #}
      }
    } 
  }
  if {[info exists w9]} {
    # w9 is --post
    set html [::http::data $http]
  } else {
    # no --post
    set html [::http::data $http]
    if {![string equal -nocase "utf-8" $state(charset)]} {
      # Non utf-8 character set found. Convert to unicode.
      if {[lsearch -exact [encoding names] $state(charset)] != -1} {
        set html [encoding convertfrom $state(charset) $html]
      }
    } else {
      if {[lsearch -exact [encoding names] $state(charset)] != -1} {
        set html [encoding convertto $state(charset) $html]
      }
    }
  }
  set bl [string bytelength $html]
  set nc [::http::ncode $http] ; set flaw "" 
  if {![info exists webbyNoGzip]} {
    # Gzip is available.
    foreach {name value} $state(meta) {
      if {[regexp -nocase ^Content-Encoding$ $name]} {
        # content-encoding meta found.
        if {[string equal -nocase "gzip" $value] && [string length $html]} {
          # encoding is gzip.
          if {![info exists webbyTrf]} {
            # no trf available.
            set bl "$bl bytes (gzip); [string bytelength [set html [zlib inflate [string range $html 10 [expr { [string length $html] - 8 } ]]]]]"
          } else {
            # trf available.
            set bl "$bl bytes (gzip); [string bytelength [set html [zip -mode decompress -nowrap 1 [string range $html 10 [expr { [string length $html] - 8 } ]]]]]"
          }
          break
        }
      }
    }
  }
  # Grab character set from html.
  if {[regexp -nocase {"Content-Type" content=".*?; charset=(.*?)".*?>} $html - char]} {
    set char [string trim [string trim $char "\"' /"] {;}]
    regexp {^(.*?)"} $char - char
    set mset $char
    if {![string length $char]} { set char "None Given" ; set char2 "None Given" }
    set char2 [string tolower [string map -nocase {"UTF-" "utf-" "iso-" "iso" "windows-" "cp" "shift_jis" "shiftjis"} $char]]
  } else {
    if {[regexp -nocase {<meta content=".*?; charset=(.*?)".*?>} $html - char]} {
      set char [string trim $char "\"' /"]
      regexp {^(.*?)"} $char - char
      set mset $char
      if {![string length $char]} { set char "None Given" ; set char2 "None Given" }
      set char2 [string tolower [string map -nocase {"UTF-" "utf-" "iso-" "iso" "windows-" "cp" "shift_jis" "shiftjis"} $char]]
    } else {
      set char "None Given" ; set char2 "None Given" ; set mset "None Given"
    }
  }
  set char3 [string tolower [string map -nocase {"UTF-" "utf-" "iso-" "iso" "windows-" "cp" "shift_jis" "shiftjis"} $state(charset)]]
  if {[string equal $char $state(charset)] && [string equal $char $char2] && ![string equal -nocase "none given" $char]} {
    # no character set conflicts.
    set char [string trim $state(charset) {;}]
    set flaw ""
  } else {
    # Conflicting character sets likely. Attempt resolution.
    if {![string equal -nocase $char2 $char3] && ![string equal -nocase "none given" $char2] && $::webbyFixDetection > 0} {
      # character sets are definitly not the same, and not set to none.
      switch $::webbyFixDetection {
        1 {if {![info exists w8]} {
            # w8 is --swap which isn't in use.
            if {$::webbyShowMisdetection > 0} {
              set flaw "\002webby\002: conflict! html meta tagging reports: $char2 .. using charset detected from http-package: $char3 to avoid conflict."
            }
            set html [webbyConflict $html $char2 $char3 [info exists w9]]
            set char [string trim $char3 {;}]
          } else {
            # --swap in use.
            if {$::webbyShowMisdetection > 0} {
              set flaw "\002webby\002: conflict! http-package reports: $char3 .. using charset detected from html meta tagging: $char2 to avoid conflict."
            }
            set html [webbyConflict $html $char3 $char2 [info exists w9]]
            set char [string trim $char2 {;}]
          }
        }
        2 {if {![info exists w8]} {
            # w8 is --swap. Is not in use.
            if {$::webbyShowMisdetection > 0} {
              set flaw "\002webby\002: conflict! http-package reports: $char3 .. using charset detected from html meta tagging: $char2 to avoid conflict."
            }
            set html [webbyConflict $html $char3 $char2 [info exists w9]]
            set char [string trim $char2 {;}]
          } else {
            # --swap in use.
            if {$::webbyShowMisdetection > 0} {
              set flaw "\002webby\002: conflict! html meta tagging reports: $char2 .. using charset detected from http-package: $char3 to avoid conflict."
            }
            set html [webbyConflict $html $char2 $char3 [info exists w9]]
            set char [string trim $char3 {;}]
          }
        }
      }
    } else {
      # char sets are the same or char2 is 'none given' or webbyFixDetection is off.
      set char [string trim $char3 {;}]
      set flaw ""
    } 
  }
  set s [list] ; set sx [list] ; set type "\( $nc" ; set metas [list]
  if {[string equal -nocase "none given" $char]} { set char [string trim $state(charset) {;}] }
  set cset $state(charset)
  switch $::webbyDynEnc {
    2 {
      # force encoding to whatever ::webbyEnc is set to.
      set html [encoding convertto [string map -nocase {"UTF-" "utf-" "iso-" "iso" "windows-" "cp" "shift_jis" "shiftjis"} $::webbyEnc]]
    }
  }
  set red ""; if {$r > 0} { set red "; $r redirects" } ;# set number of redirects and create redirect info msg.
  foreach {name value} $state(meta) {
    if {[string match -nocase "content-type" $name]} {
      # content type found. grab value and format type info msg with charset 
      set spltval [split $value ";"]
      append type "; [lindex $spltval 0]; $char; ${bl} bytes$red"
      continue
    }
    if {[string match -nocase "set-cookie" $name]} { continue }
    if {[string match -nocase "x-*" $name]} { lappend sx "\002$name\002=$value" ; continue}
    lappend s "\002$name\002=$value"
  }
  append type " \)" ; set e ""
  ::http::cleanup $http
  regsub -all {(?:\n|\t|\v|\r|\x01)} $html " " html
     # DEBUG DEBUG                    
     set junk [open "webby.txt" w]
     puts $junk $html
     set ::delTemp $html
     close $junk
  if {![regexp -nocase {<title.*?>(.*?)</title>} $html - title]} {
    if {[info exists w10]} {
      set title "\002Validated\002: [join [lrange [split $fullquery /] 0 2] /]"
    } else {
      set title "No Title"
    }
  } else {
    while {[string match "*  *" $title]} { regsub -all -- {  } [string trim $title] " " title }
  }
  while {[regexp -nocase {<meta ((?!content).*?) content=(.*?)>} $html - name value]} {
    set name [string trim [lindex [split $name "="] end] "\"' /"]
    set value [string trim $value "\"' /"]
    lappend metas "$name=$value"
    regsub -nocase {<meta (?!content).*? content=.*?>} $html "" html
    set metaflag 1
  }
  if {![info exists metaflag]} {
    while {[regexp -nocase {<meta .*?=(.*?) name=(.*?)>} $html - value name]} {
      set name [string trim [lindex [split $name "="] end] "\"' /"]
      set value [string trim $value "\"' /"]
      lappend metas "$name=$value"
      regsub -nocase {<meta .*?=.*? name=.*?>} $html "" html
    }
  }
  if {[info exists w3]} {
    if {![regexp -nocase {<\!DOCTYPE html PUBLIC \"-//W3C//DTD (.*?)"} $html - hv]} {
      if {![regexp -nocase {<!DOCTYPE html PUBLIC \'-//W3C//DTD (.*?)'} $html - hv]} {
        if {[llength $metas]} {
          foreach line [line_wrap [join $metas "; "]] {
            append hv " \002Metas\002: $line\n"
          }
        } else { set hv "" }
      }
    } else {
      set hv "\002[lindex [split $hv] 0]\002 [join [lrange [split $hv] 1 end] " "] "
      if {[llength $metas]} { 
        foreach line [line_wrap [join $metas "; "]] {
          append hv " \002Metas\002: $line\n"
        }
      }
    }
  }
  if {[llength $metas]} {
    if {[set pos [lsearch -glob [split [string tolower [join $metas "<"]] "<"] "description=*"]] != -1} {
      set desc [lindex [split [lindex $metas $pos] =] 1]
    } else {
      set desc ""
    }
  } else { set desc "" }

  # SPAM
  if {$::webbyShowMisdetection > 0} {
    if {[string length $flaw]} { putserv "privmsg $chan :$flaw" }
    if {$::webbyShowMisdetection > 1} {
      putserv "privmsg $chan :\002webby\002: Detected \002$char\002 \:\: Http-Package: $cset -> $char3 .. Meta-Charset: $mset -> $char2 \:\:"
    }
  }
  if {($::webbyRegShow > 0) || ![info exists w5]} {
    putserv "privmsg $chan :[webbydescdecode $title $char] \( [webbytiny $fullquery $::webbyShortType] \)$type"
    if {($::webbyheader > 0 && [llength $s]) || [info exists w1]} { putserv "privmsg $chan :[join [lsort -decreasing $s] "; "] " }
    if {($::webbyXheader > 0 && [llength $sx]) || [info exists w2]} { putserv "privmsg $chan :[join [lsort -decreasing $sx] "; "] " }
    if {($::webbydoc > 0 && [string length $hv]) || [info exists w3]} {
      foreach line [split [string trim $hv] \n] { putserv "privmsg $chan :$line" }
    }
    if {![info exists w3]} {
      foreach line [line_wrap [webbydescdecode $desc $char]] {
        putserv "privmsg $chan :$line"
      }
    }
  } else {
    if {[info exists w5]} {
      switch $varnum {
        1 { catch {regexp -nocase -- "$reggy" $html - m(1)} e }
        2 { catch {regexp -nocase -- "$reggy" $html - m(1) m(2)} e }
        3 { catch {regexp -nocase -- "$reggy" $html - m(1) m(2) m(3)} e }
        4 { catch {regexp -nocase -- "$reggy" $html - m(1) m(2) m(3) m(4)} e }
        5 { catch {regexp -nocase -- "$reggy" $html - m(1) m(2) m(3) m(4) m(5)} e }
        6 { catch {regexp -nocase -- "$reggy" $html - m(1) m(2) m(3) m(4) m(5) m(6)} e }
        7 { catch {regexp -nocase -- "$reggy" $html - m(1) m(2) m(3) m(4) m(5) m(6) m(7)} e }
        8 { catch {regexp -nocase -- "$reggy" $html - m(1) m(2) m(3) m(4) m(5) m(6) m(7) m(8)} e }
        9 { catch {regexp -nocase -- "$reggy" $html - m(1) m(2) m(3) m(4) m(5) m(6) m(7) m(8) m(9)} e }
      }
      if {![string is digit $e]} {
        putserv "privmsg $chan :\002regexp\002: [set e]"
        return 0
      } else {
        set found 0
        for {set n 1} {$n<10} {incr n} {
          if {[info exists m($n)]} {
            incr found
            if {([string length $m($n)] < $::regexp_limit) || [info exists w6]} {
              if {![info exists w7]} { set m($n) [unhtml $m($n)] }
              foreach line [line_wrap [webbydescdecode $m($n) $char]] {
                putserv "privmsg $chan :\002regexp\002: capture$n ( $line \017)"
              }
            } else {
              putserv "privmsg $chan :\002regexp\002: capture$n is too long for display to irc ([string length $m($n)] > $::regexp_limit)."
            }
          }
        }
        if {$found == 0} {
           putserv "privmsg $chan :\002regexp\002: does not match any html."
        }
      }
    }
  }
}

proc webbyConflict {html in out gz} {
  if {$gz > 0 } { return $html }
  if {![string equal $in [encoding system]]} { set html [encoding convertto $in $html] }
  if {![string equal $out [encoding system]]} { set html [encoding convertfrom $out $html] }
  return $html
}
proc unhtml {text} {
  regsub -all "(?:<b>|</b>|<b />|<em>|</em>|<strong>|</strong>)" $text "\002" text
  regsub -all "(?:<u>|</u>|<u />)" $text "\037" text
  regsub -all "(?:<br>|<br/>|<br />)" $text ". " text
  regsub -all "<script.*?>.*?</script>" $text "" text
  regsub -all "<style.*?>.*?</style>" $text "" text
  regsub -all -- {<.*?>} $text " " text
  while {[string match "*  *" $text]} { regsub -all "  " $text " " text }
  return [string trim $text]
}

proc webbydescdecode {text char} {
   # code below is neccessary to prevent numerous html markups
   # from appearing in the output (ie, &quot;, &#5671;, etc)
   # stolen (borrowed is a better term) from perplexa's urban
   # dictionary script..
   if {![string match *&* $text]} {return $text}
   if {[string match "*;*" $char]} {set char [string trim $char {;}] }
   set escapes {
		&nbsp; \xa0 &iexcl; \xa1 &cent; \xa2 &pound; \xa3 &curren; \xa4
		&yen; \xa5 &brvbar; \xa6 &sect; \xa7 &uml; \xa8 &copy; \xa9
		&ordf; \xaa &laquo; \xab &not; \xac &shy; \xad &reg; \xae
		&macr; \xaf &deg; \xb0 &plusmn; \xb1 &sup2; \xb2 &sup3; \xb3
		&acute; \xb4 &micro; \xb5 &para; \xb6 &middot; \xb7 &cedil; \xb8
		&sup1; \xb9 &ordm; \xba &raquo; \xbb &frac14; \xbc &frac12; \xbd
		&frac34; \xbe &iquest; \xbf &Agrave; \xc0 &Aacute; \xc1 &Acirc; \xc2
		&Atilde; \xc3 &Auml; \xc4 &Aring; \xc5 &AElig; \xc6 &Ccedil; \xc7
		&Egrave; \xc8 &Eacute; \xc9 &Ecirc; \xca &Euml; \xcb &Igrave; \xcc
		&Iacute; \xcd &Icirc; \xce &Iuml; \xcf &ETH; \xd0 &Ntilde; \xd1
		&Ograve; \xd2 &Oacute; \xd3 &Ocirc; \xd4 &Otilde; \xd5 &Ouml; \xd6
		&times; \xd7 &Oslash; \xd8 &Ugrave; \xd9 &Uacute; \xda &Ucirc; \xdb
		&Uuml; \xdc &Yacute; \xdd &THORN; \xde &szlig; \xdf &agrave; \xe0
		&aacute; \xe1 &acirc; \xe2 &atilde; \xe3 &auml; \xe4 &aring; \xe5
		&aelig; \xe6 &ccedil; \xe7 &egrave; \xe8 &eacute; \xe9 &ecirc; \xea
		&euml; \xeb &igrave; \xec &iacute; \xed &icirc; \xee &iuml; \xef
		&eth; \xf0 &ntilde; \xf1 &ograve; \xf2 &oacute; \xf3 &ocirc; \xf4
		&otilde; \xf5 &ouml; \xf6 &divide; \xf7 &oslash; \xf8 &ugrave; \xf9
		&uacute; \xfa &ucirc; \xfb &uuml; \xfc &yacute; \xfd &thorn; \xfe
		&yuml; \xff &fnof; \u192 &Alpha; \u391 &Beta; \u392 &Gamma; \u393 &Delta; \u394
		&Epsilon; \u395 &Zeta; \u396 &Eta; \u397 &Theta; \u398 &Iota; \u399
		&Kappa; \u39A &Lambda; \u39B &Mu; \u39C &Nu; \u39D &Xi; \u39E
		&Omicron; \u39F &Pi; \u3A0 &Rho; \u3A1 &Sigma; \u3A3 &Tau; \u3A4
		&Upsilon; \u3A5 &Phi; \u3A6 &Chi; \u3A7 &Psi; \u3A8 &Omega; \u3A9
		&alpha; \u3B1 &beta; \u3B2 &gamma; \u3B3 &delta; \u3B4 &epsilon; \u3B5
		&zeta; \u3B6 &eta; \u3B7 &theta; \u3B8 &iota; \u3B9 &kappa; \u3BA
		&lambda; \u3BB &mu; \u3BC &nu; \u3BD &xi; \u3BE &omicron; \u3BF
		&pi; \u3C0 &rho; \u3C1 &sigmaf; \u3C2 &sigma; \u3C3 &tau; \u3C4
		&upsilon; \u3C5 &phi; \u3C6 &chi; \u3C7 &psi; \u3C8 &omega; \u3C9
		&thetasym; \u3D1 &upsih; \u3D2 &piv; \u3D6 &bull; \u2022
		&hellip; \u2026 &prime; \u2032 &Prime; \u2033 &oline; \u203E
		&frasl; \u2044 &weierp; \u2118 &image; \u2111 &real; \u211C
		&trade; \u2122 &alefsym; \u2135 &larr; \u2190 &uarr; \u2191
		&rarr; \u2192 &darr; \u2193 &harr; \u2194 &crarr; \u21B5
		&lArr; \u21D0 &uArr; \u21D1 &rArr; \u21D2 &dArr; \u21D3 &hArr; \u21D4
		&forall; \u2200 &part; \u2202 &exist; \u2203 &empty; \u2205
		&nabla; \u2207 &isin; \u2208 &notin; \u2209 &ni; \u220B &prod; \u220F
		&sum; \u2211 &minus; \u2212 &lowast; \u2217 &radic; \u221A
		&prop; \u221D &infin; \u221E &ang; \u2220 &and; \u2227 &or; \u2228
		&cap; \u2229 &cup; \u222A &int; \u222B &there4; \u2234 &sim; \u223C
		&cong; \u2245 &asymp; \u2248 &ne; \u2260 &equiv; \u2261 &le; \u2264
		&ge; \u2265 &sub; \u2282 &sup; \u2283 &nsub; \u2284 &sube; \u2286
		&supe; \u2287 &oplus; \u2295 &otimes; \u2297 &perp; \u22A5
		&sdot; \u22C5 &lceil; \u2308 &rceil; \u2309 &lfloor; \u230A
		&rfloor; \u230B &lang; \u2329 &rang; \u232A &loz; \u25CA
		&spades; \u2660 &clubs; \u2663 &hearts; \u2665 &diams; \u2666
		&quot; \x22 &amp; \x26 &lt; \x3C &gt; \x3E O&Elig; \u152 &oelig; \u153
		&Scaron; \u160 &scaron; \u161 &Yuml; \u178 &circ; \u2C6
		&tilde; \u2DC &ensp; \u2002 &emsp; \u2003 &thinsp; \u2009
		&zwnj; \u200C &zwj; \u200D &lrm; \u200E &rlm; \u200F &ndash; \u2013
		&mdash; \u2014 &lsquo; \u2018 &rsquo; \u2019 &sbquo; \u201A
		&ldquo; \u201C &rdquo; \u201D &bdquo; \u201E &dagger; \u2020
		&Dagger; \u2021 &permil; \u2030 &lsaquo; \u2039 &rsaquo; \u203A
		&euro; \u20AC &apos; \u0027 &lrm; "" &rlm; "" &#8236; "" &#8237; ""
		&#8238; "" &#8212; \u2014
  };
  if {![string equal $char [encoding system]]} { set text [encoding convertfrom $char $text] }
  set text [string map [list "\]" "\\\]" "\[" "\\\[" "\$" "\\\$" "\\" "\\\\"] [string map $escapes $text]]
  regsub -all -- {&#([[:digit:]]{1,5});} $text {[format %c [string trimleft "\1" "0"]]} text
  regsub -all -- {&#x([[:xdigit:]]{1,4});} $text {[format %c [scan "\1" %x]]} text
  regsub -all -- {\\x([[:xdigit:]]{1,2})} $text {[format %c [scan "\1" %x]]} text
  set text [subst "$text"]
  if {![string equal $char [encoding system]]} { set text [encoding convertto $char $text] }
  return $text
}

proc webbytiny {url type} {
  set ua "Mozilla/5.0 (Windows; U; Windows NT 5.1; en-US; rv:1.9.0.5) Gecko/2008120122 Firefox/3.0.5"
  set http [::http::config -useragent $ua -urlencoding "utf-8"]
  switch -- $type {
    4 { set type [rand 4] }
    5 { if {![info exists ::webbyCount]} {
          set ::webbyCount 0
          set type 0
        } else {
          set type [expr {[incr ::webbyCount] % 4}]
        }
      }
  } 
  switch -- $type {
    0 { set query "http://tinyurl.com/api-create.php?[http::formatQuery url $url]" }
    1 { set query "http://is.gd/api.php?[http::formatQuery longurl $url]" }
    2 { set query "http://is.gd/api.php?[http::formatQuery longurl $url]" }
    3 { set query "http://is.gd/api.php?[http::formatQuery longurl $url]" }
  }
  set token [http::geturl $query -timeout 3000]
  upvar #0 $token state
  if {[string length $state(body)]} { return [string map {"\n" ""} $state(body)] }
  return $url
}

# LINE_WRAP
# takes a long line in, and chops it before the specified length
# http://forum.egghelp.org/viewtopic.php?t=6690
#
proc line_wrap {str {splitChr { }}} { 
  set out [set cur {}]
  set i 0
  set len $::webbySplit
  foreach word [split [set str][set str ""] $splitChr] { 
    if {[incr i [string length $word]] > $len} { 
      lappend out [join $cur $splitChr] 
      set cur [list $word] 
      set i [string length $word] 
    } else { 
      lappend cur $word 
    } 
    incr i 
  } 
  lappend out [join $cur $splitChr] 
}

#  idna support
#  
#      This file is part of the jabberlib. It provides support for
#      Internationalizing Domain Names in Applications (IDNA, RFC 3490).
#      
#  Copyright (c) 2005 Alexey Shchepin <alexey@sevcom.net>
#  
# $Id$
#
#  SYNOPSIS
#      idna::domain_toascii domain
#

namespace eval idna {}

##########################################################################

proc idna::domain_toascii {domain} {
  #set domain [string tolower $domain]
  set parts [split $domain "\u002E\u3002\uFF0E\uFF61"]
  set res {}
  foreach p $parts {
    set r [toascii $p]
    lappend res $r
  }
  return [join $res .]
}

##########################################################################

proc idna::toascii {name} {
  # TODO: Steps 2, 3 and 5 from RFC3490
  if {![string is ascii $name]} {
    set name [punycode_encode $name]
    set name "xn--$name"
  }
  return $name
}

##########################################################################

proc idna::punycode_encode {input} {
  set base 36
  set tmin 1
  set tmax 26
  set skew 38
  set damp 700
  set initial_bias 72
  set initial_n 0x80
  set n $initial_n
  set delta 0
  set out 0
  set bias $initial_bias
  set output ""
  set input_length [string length $input]
  set nonbasic {}
  for {set j 0} {$j < $input_length} {incr j} {
    set c [string index $input $j]
    if {[string is ascii $c]} {
      append output $c
    } else {
      lappend nonbasic $c
    }
  }
  set nonbasic [lsort -unique $nonbasic]
  set h [set b [string length $output]];
  if {$b > 0} {
    append output -
  }

  while {$h < $input_length} {
    set m [scan [string index $nonbasic 0] %c]
    set nonbasic [lrange $nonbasic 1 end]
    incr delta [expr {($m - $n) * ($h + 1)}]
    set n $m
    for {set j 0} {$j < $input_length} {incr j} {
	    set c [scan [string index $input $j] %c]
	    if {$c < $n} {
        incr delta
	    } elseif {$c == $n} {
        for {set q $delta; set k $base} {1} {incr k $base} {
          set t [expr {$k <= $bias ? $tmin :
          $k >= $bias + $tmax ? $tmax : $k - $bias}]
          if {$q < $t} break;
          append output [punycode_encode_digit [expr {$t + ($q - $t) % ($base - $t)}]]
          set q [expr {($q - $t) / ($base - $t)}]
        }
        append output [punycode_encode_digit $q]
        set bias [punycode_adapt $delta [expr {$h + 1}] [expr {$h == $b}]]
        set delta 0
        incr h
      }
    }
    incr delta
    incr n
  }
  return $output;
}

##########################################################################

proc idna::punycode_adapt {delta numpoints firsttime} {
  set base 36
  set tmin 1
  set tmax 26
  set skew 38
  set damp 700
  set delta [expr {$firsttime ? $delta / $damp : $delta >> 1}]
  incr delta [expr {$delta / $numpoints}]
  for {set k 0} {$delta > (($base - $tmin) * $tmax) / 2}  {incr k $base} {
    set delta [expr {$delta / ($base - $tmin)}];
  }
  return [expr {$k + ($base - $tmin + 1) * $delta / ($delta + $skew)}]
}

##########################################################################

proc idna::punycode_encode_digit {d} {
  sreturn [format %c [expr {$d + 22 + 75 * ($d < 26)}]]
}

##########################################################################

putlog "Durby v0.1 has been loaded."