;; This is my font configuration file. It sets up cjk fonts that can be
;; difficult, and uses Linux Libertine at the right sizes.
;; I copied the majority from https://idiocy.org/emacs-fonts-and-fontsets.html

(set-face-attribute 'default nil
                    :family "Iosevka Nerd Font"
		    :weight 'normal
		    :height 120)

(set-face-attribute 'fixed-pitch nil
		    :family "Iosevka Nerd Font"
		    :height 1.0)

(set-face-attribute 'italic nil
		    :underline nil
                    :slant 'italic)

(set-face-attribute 'variable-pitch nil
                    :font "DejaVu Serif"
                    :height 1.1)

(set-face-attribute 'fixed-pitch-serif nil
                    :font "Go Mono"
                    :height 1.0)

;; Misc
(set-fontset-font t 'gothic "Noto Sans Gothic")
(set-fontset-font t 'unicode "Noto Color Emoji" nil 'append)
(set-fontset-font t 'unicode "Free Mono" nil 'append)

;; normal-ish scripts that aren't using the default font
(set-fontset-font t 'cyrillic "Iosevka") ; русский
(set-fontset-font t 'greek "Iosevka")	; ελληνικά
(set-fontset-font t 'braille "Iosevka") ; ⠓⠑⠇⠇⠕
(set-fontset-font t 'phonetic "Iosevka") ; /ˈɪŋɡlɪʃ/

;; East Asia: 你好, 早晨, こんにちは, 안녕하세요
;;
;; Make sure you use the right font. See
;; https://www.google.com/get/noto/help/cjk/.
(set-fontset-font t 'han "Sarasa Mono HC")
(set-fontset-font t 'kana "Sarasa Mono J")
(set-fontset-font t 'hangul "Sarasa Mono K")
(set-fontset-font t 'cjk-misc "Sarasa Mono CL")

;; South East Asia: ជំរាបសួរ, ສະບາຍດີ, မင်္ဂလာပါ, สวัสดีครับ
(set-fontset-font t 'khmer "Noto Sans Khmer")
(set-fontset-font t 'lao "Noto Sans Lao")
(set-fontset-font t 'burmese "Noto Sans Myanmar")
(set-fontset-font t 'thai "Noto Sans Thai")

;; Africa: ሠላም
(set-fontset-font t 'ethiopic "Noto Sans Ethiopic")

;; Middle/Near East: שלום, السّلام عليكم
(set-fontset-font t 'hebrew "Noto Sans Hebrew")
(set-fontset-font t 'arabic "Noto Sans Arabic")


;;  South Asia: નમસ્તે, नमस्ते, ನಮಸ್ಕಾರ, നമസ്കാരം, ଶୁଣିବେ,
;;              ආයුබෝවන්, வணக்கம், నమస్కారం, བཀྲ་ཤིས་བདེ་ལེགས༎
(set-fontset-font t 'gujarati "Noto Sans Gujarati")
(set-fontset-font t 'devanagari "Noto Sans Devanagari")
(set-fontset-font t 'kannada "Noto Sans Kannada")
(set-fontset-font t 'malayalam "Noto Sans Malayalam")
(set-fontset-font t 'oriya "Noto Sans Oriya")
(set-fontset-font t 'sinhala "Noto Sans Sinhala")
(set-fontset-font t 'tamil "Noto Sans Tamil")
(set-fontset-font t 'telugu "Noto Sans Telugu")
(set-fontset-font t 'tibetan "Noto Sans Tibetan")
(set-fontset-font t 'bengali "Noto Sans Bengali")


(set-fontset-font t 'cherokee "Noto Sans Cherokee")
(set-fontset-font t 'georgian "Noto Sans Georgian")
(set-fontset-font t 'armenian "Noto Sans Armenian")

(set-fontset-font t 'tai-viet "Noto Sans Tai Viet")
