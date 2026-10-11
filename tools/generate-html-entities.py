#!/usr/bin/env python3
"""Regenerate src/kracht/html-entities.scm.

The entity tables in that module are data: rather than transcribing a couple of
thousand named character references by hand, this script writes them out from
the standard tables, which Python bundles.  It needs nothing but a Python 3
interpreter and writes the module in place.

    python3 tools/generate-html-entities.py

Sources:
  * html.entities.html5       -- the WHATWG named character reference list, the
                                 same data the HTML standard publishes as
                                 https://html.spec.whatwg.org/entities.json
  * html.entities.codepoint2name -- the HTML4 set, which is what kracht is
                                 willing to emit as an entity when escaping
"""
import html.entities as e

HEADER = '''\
;; kracht

;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;; kracht is free software: you can redistribute it and/or modify
;; it under the terms of the Lesser GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; kracht is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; Lesser GNU General Public License for more details.

;; You should have received a copy of the GNU Lesser General Public License
;; along with kracht.  If not, see <https://www.gnu.org/licenses/>.

;; This module is data, and it is generated rather than written by hand.  The
;; named character references below are the ones the HTML standard defines --
;; the WHATWG list published as
;;   https://html.spec.whatwg.org/entities.json
;; and bundled with Python as html.entities.html5.  Regenerate the file with
;; tools/generate-html-entities.py rather than editing it.
;;
;; Only references that end in a semicolon are kept.  The HTML parser also
;; accepts a legacy set of unterminated forms (`&amp' with no `;'), but only in
;; positions where no other character could continue the name; kracht's decoder
;; requires the terminator instead, which is unambiguous everywhere.

(define-module (kracht html-entities)
  #:export (html-named-entities
            html-escape-entities
            html-entity-ref
            html-char-ref))

;; Every HTML5 named character reference that ends in `;', as an association
;; list from the reference as it appears in a document to the characters it
;; stands for.  The value is a string rather than a character because 93 of
;; these references -- `&NotEqualTilde;', say -- stand for two code points.
(define html-named-entities
  '(
'''


def scheme_string(s):
    out = s.replace("\\", "\\\\").replace('"', '\\"')
    return '"' + out + '"'


def main():
    decode = sorted(
        ("&" + k, v) for k, v in e.html5.items() if k.endswith(";")
    )
    decoded = dict(decode)

    encode = []
    for codepoint, name in sorted(e.codepoint2name.items()):
        encode.append((chr(codepoint), "&" + name + ";"))
    # HTML4 has no apostrophe entity; the HTML5 one is what every other
    # reference we write belongs to, so use it for consistency.
    encode = [(c, ref) for c, ref in encode if c != "'"]
    encode.append(("'", "&apos;"))

    # Only keep a character if writing its entity is safe, that is if decoding
    # that same reference gives the character back.  HTML4 and HTML5 disagree
    # about two of them: `&lang;' and `&rang;' name the CJK angle brackets in
    # HTML4 but the mathematical angle brackets (U+27E8/U+27E9) in HTML5, so
    # writing the entity would silently change the character.  Those are left
    # out and pass through as themselves.
    kept, dropped = [], []
    for char, ref in encode:
        if decoded.get(ref) == char:
            kept.append((char, ref))
        else:
            dropped.append((char, ref, decoded.get(ref)))
    encode = sorted(kept, key=lambda pair: ord(pair[0]))

    parts = [HEADER]
    for ref, value in decode:
        parts.append("    (%s . %s)\n" % (scheme_string(ref), scheme_string(value)))
    parts.append("    ))\n\n")

    parts.append(
        ";; The characters `html-escape' writes as named entities: the HTML4 set,\n"
        ";; which is Latin-1 plus the common symbols, together with the five\n"
        ";; characters HTML is sensitive to and `&apos;'.  Decoding is deliberately\n"
        ";; broader than encoding -- this is the list worth writing, while\n"
        ";; `html-named-entities' is the list worth understanding.\n"
        ";;\n"
        ";; `&lang;' and `&rang;' are the two HTML4 names deliberately left out:\n"
        ";; HTML5 reads them as the mathematical angle brackets U+27E8 and U+27E9\n"
        ";; rather than the CJK ones U+2329 and U+232A, so a character written\n"
        ";; with either entity would not come back from decoding unchanged.\n"
        "(define html-escape-entities\n  '(\n"
    )
    for char, ref in encode:
        parts.append("    (%s . %s)\n" % (scheme_string(char), scheme_string(ref)))
    parts.append("    ))\n\n")

    parts.append(
        ";; Both tables are looked up by key, so build the hash tables once here\n"
        ";; rather than scanning an alist per character.\n"
        "(define html-entity-lookup\n"
        "  (let ((table (make-hash-table)))\n"
        "    (for-each (lambda (entry)\n"
        "                (hash-set! table (car entry) (cdr entry)))\n"
        "              html-named-entities)\n"
        "    table))\n\n"
        "(define html-char-lookup\n"
        "  (let ((table (make-hash-table)))\n"
        "    (for-each (lambda (entry)\n"
        "                (hash-set! table (string-ref (car entry) 0) (cdr entry)))\n"
        "              html-escape-entities)\n"
        "    table))\n\n"
        "(define (html-entity-ref name)\n"
        '  "Return the characters the named character reference NAME stands for, or\n'
        "#f when NAME is not one kracht knows.\n\n"
        "NAME is the reference as it appears in the document, including the\n"
        "leading ampersand and the trailing semicolon -- `\\\"&aacute;\\\"', not\n"
        "`\\\"aacute\\\"' or `\\\"&aacute\\\"'.\"\n"
        "  (hash-ref html-entity-lookup name #f))\n\n"
        "(define (html-char-ref c)\n"
        '  "Return the named character reference for the character C, or #f when\n'
        "kracht has no entity it is worth writing for C.\"\n"
        "  (hash-ref html-char-lookup c #f))\n"
    )

    with open("src/kracht/html-entities.scm", "w", encoding="utf-8") as handle:
        handle.write("".join(parts))

    print("decode entries: %d" % len(decode))
    print("encode entries: %d" % len(encode))
    print("multi-codepoint: %d" % sum(1 for _, v in decode if len(v) > 1))
    for char, ref, value in dropped:
        print(
            "dropped %s (%s): decodes to %s, not the character"
            % (ref, hex(ord(char)), hex(ord(value)) if value else "nothing")
        )


main()
