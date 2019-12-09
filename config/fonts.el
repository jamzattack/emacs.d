;; This is my font configuration file. It sets up cjk fonts that can be
;; difficult, and uses Linux Libertine at the right sizes.
;; I copied the majority from https://idiocy.org/emacs-fonts-and-fontsets.html

(setq my-default-font "Linux Libertine Mono O")

(set-face-attribute 'default nil
                    :font (concat my-default-font " 8"))
(set-face-attribute 'variable-pitch nil
                    :font "Linux Libertine O"
                    :height 1.1)
(set-face-attribute 'fixed-pitch-serif nil
                    :font "Noto Sans Mono"
                    :height 1.0)

;; Latin
(set-fontset-font t 'latin "Linux Libertine Mono O")

;; East Asia: 你好, 早晨, こんにちは, 안녕하세요
;;
;; Make sure you use the right font. See
;; https://www.google.com/get/noto/help/cjk/.
(set-fontset-font t 'han "Noto Sans CJK SC Medium")
(set-fontset-font t 'kana "Noto Sans CJK JP Medium")
(set-fontset-font t 'hangul "Noto Sans CJK KR Medium")
(set-fontset-font t 'cjk-misc "Noto Sans CJK KR Bold")

(set-fontset-font t 'unicode "Symbola" nil 'append)
(set-fontset-font t 'unicode "Noto Emoji" nil 'append)

(set-fontset-font t 'cyrillic my-default-font)

;; South East Asia: ជំរាបសួរ, ສະບາຍດີ, မင်္ဂလာပါ, สวัสดีครับ
(set-fontset-font t 'khmer "Noto Serif Khmer Medium")
(set-fontset-font t 'lao "Noto Sans Lao Medium")
(set-fontset-font t 'burmese "Noto Sans Myanmar Medium")
(set-fontset-font t 'thai "Noto Sans Thai Medium")

;; Africa: ሠላም
(set-fontset-font t 'ethiopic "Noto Sans Ethiopic Medium")

;; Middle/Near East: שלום, السّلام عليكم
(set-fontset-font t 'hebrew "Noto Serif Hebrew Medium")
(set-fontset-font t 'arabic "Noto Sans Arabic Medium")

;; (set-fontset-font t 'hebrew my-default-font)
;; (set-fontset-font t 'arabic my-default-font)


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
