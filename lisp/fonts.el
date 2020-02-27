;; This is my font configuration file. It sets up cjk fonts that can be
;; difficult, and uses Linux Libertine at the right sizes.
;; I copied the majority from https://idiocy.org/emacs-fonts-and-fontsets.html

(set-face-attribute 'default nil
                    :font "Go Mono"
		    :weight 'normal
		    :height 80)

(set-face-attribute 'italic nil
		    :underline nil
                    :slant 'italic)

(set-face-attribute 'variable-pitch nil
                    :font "Noto Serif"
                    :height 1.1)

(set-face-attribute 'fixed-pitch-serif nil
                    :font "DejaVu Sans Mono"
                    :height 1.0)

;; East Asia: 你好, 早晨, こんにちは, 안녕하세요
;;
;; Make sure you use the right font. See
;; https://www.google.com/get/noto/help/cjk/.
(set-fontset-font t 'han "Noto Sans Mono CJK SC")
(set-fontset-font t 'kana "Noto Sans Mono CJK JP")
(set-fontset-font t 'hangul "Noto Sans Mono CJK KR")
(set-fontset-font t 'cjk-misc "Noto Sans Mono CJK KR Bold")

(set-fontset-font t 'unicode "Noto Color Emoji" nil 'append)

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
















