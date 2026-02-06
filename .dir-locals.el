;;; SPDX-FileCopyrightText: 2026 Richard Brežák <magic_rb@redalder.org> and NixNG contributors
;;;
;;; SPDX-License-Identifier: CC0-1.0

(
 ( nil . ( ( fill-column . 80 ) ) )
 ( haskell-mode ( eglot-workspace-configuration .
                  ( :haskell ( :plugin ( :hlint ( :globalOn :json-false ) )
                               :formattingProvider "fourmolu" )))))
