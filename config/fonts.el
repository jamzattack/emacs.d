;; This is my font configuration file. It sets up cjk fonts that can be
;; difficult, and uses Input Mono & Sans at the right sizes.
;; I copied the majority from https://idiocy.org/emacs-fonts-and-fontsets.html

(set-face-attribute 'default nil
                    :font "Input Mono Light 8")
(set-face-attribute 'variable-pitch nil
                    :font "Input Sans"
                    :height 1.0)
(set-face-attribute 'fixed-pitch-serif nil
                    :font "Go Mono"
                    :height 1.0)

;; Latin
(set-fontset-font t 'latin "Noto Sans")

;; East Asia: 你好, 早晨, こんにちは, 안녕하세요
;;
;; Make sure you use the right font. See
;; https://www.google.com/get/noto/help/cjk/.
(set-fontset-font t 'han "Noto Sans CJK SC Medium")
(set-fontset-font t 'kana "Noto Sans CJK JP Medium")
(set-fontset-font t 'hangul "Noto Sans CJK KR Medium")
(set-fontset-font t 'cjk-misc "Noto Sans CJK KR Bold")

(set-fontset-font t 'unicode "Symbola" nil 'append)

(set-fontset-font t 'cyrillic "Input Mono")

;; South East Asia: ជំរាបសួរ, ສະບາຍດີ, မင်္ဂလာပါ, สวัสดีครับ
(set-fontset-font t 'khmer "Noto Serif Khmer Medium")
(set-fontset-font t 'lao "Noto Sans Lao Medium")
(set-fontset-font t 'burmese "Noto Sans Myanmar Medium")
(set-fontset-font t 'thai "Noto Sans Thai Medium")

;; Africa: ሠላም
(set-fontset-font t 'ethiopic "Noto Sans Ethiopic Medium")

;; Middle/Near East: שלום, السّلام عليكم
(set-fontset-font t 'hebrew "Noto Sans Hebrew Medium")
(set-fontset-font t 'arabic "Noto Sans Arabic Medium")

;;  South Asia: નમસ્તે, नमस्ते, ನಮಸ್ಕಾರ, നമസ്കാരം, ଶୁଣିବେ,
;;              ආයුබෝවන්, வணக்கம், నమస్కారం, བཀྲ་ཤིས་བདེ་ལེགས༎
(set-fontset-font t 'gujarati "Noto Sans Gujarati Medium")
(set-fontset-font t 'devanagari "Noto Sans Devanagari Medium")
(set-fontset-font t 'kannada "Noto Sans Kannada Medium")
(set-fontset-font t 'malayalam "Noto Sans Malayalam Medium")
(set-fontset-font t 'oriya "Noto Sans Oriya Medium")
(set-fontset-font t 'sinhala "Noto Sans Sinhala Medium")
(set-fontset-font t 'tamil "Noto Sans Tamil Medium")
(set-fontset-font t 'telugu "Noto Sans Telugu Medium")
(set-fontset-font t 'tibetan "Noto Sans Tibetan Medium")
