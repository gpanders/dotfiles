(local api vim.api)
(local present (require :present))
(local namespace (api.nvim_create_namespace "tool/annotate"))
(local quickfix-context {:pi_annotations true})
(local state {:buffers {}
              :groups {}
              :quickfix nil})

(fn define-highlights []
  (api.nvim_set_hl 0 :PiAnnotationSign {:default true :link :SpecialComment})
  (api.nvim_set_hl 0 :PiAnnotationUnderline {:default true :link :Underlined}))

(define-highlights)
(api.nvim_create_autocmd :ColorScheme
                         {:group (api.nvim_create_augroup :pi_annotation_highlights {:clear true})
                          :callback define-highlights})

(fn quickfix-id [create?]
  (when state.quickfix
    (let [list (vim.fn.getqflist {:id state.quickfix :context 0})]
      (when (not (and (= list.id state.quickfix)
                      (vim.deep_equal list.context quickfix-context)))
        (set state.quickfix nil))))
  (when (not state.quickfix)
    (for [nr (or (. (vim.fn.getqflist {:nr :$}) :nr) 0) 1 -1 &until state.quickfix]
      (let [list (vim.fn.getqflist {:nr nr :id 0 :context 0})]
        (when (vim.deep_equal list.context quickfix-context)
          (set state.quickfix list.id)))))
  (when (and (not state.quickfix) create?)
    (assert (= 0 (vim.fn.setqflist [] " " {:nr :$
                                             :title "Annotations"
                                             :context quickfix-context
                                             :items []}))
            "Neovim could not create the annotation quickfix list")
    (set state.quickfix (. (vim.fn.getqflist {:nr :$ :id 0}) :id)))
  state.quickfix)

(fn update-quickfix []
  (let [items []]
    (each [buf annotations (pairs state.buffers)]
      (when (api.nvim_buf_is_valid buf)
        (each [_ annotation (ipairs annotations)]
          (let [position (api.nvim_buf_get_extmark_by_id buf namespace annotation.mark {})]
            (when (= (length position) 2)
              (table.insert items
                            {:bufnr buf
                             :lnum (+ (. position 1) 1)
                             :col (+ (. position 2) 1)
                             :text (or (. (api.nvim_buf_get_lines buf (. position 1) (+ (. position 1) 1) false) 1)
                                       "")}))))))
    (table.sort items
                (fn [left right]
                  (let [left-name (api.nvim_buf_get_name left.bufnr)
                        right-name (api.nvim_buf_get_name right.bufnr)]
                    (if (not= left-name right-name)
                        (< left-name right-name)
                        (= left.lnum right.lnum)
                        (< left.col right.col)
                        (< left.lnum right.lnum)))))
    (let [id (quickfix-id (< 0 (length items)))]
      (assert (or (not id)
                  (= 0 (vim.fn.setqflist [] "u" {:id id
                                                   :title "Annotations"
                                                   :context quickfix-context
                                                   :items items})))
              "Neovim could not update the annotation quickfix list"))))

(fn rebuild-buffers []
  (set state.buffers {})
  (each [_ annotations (pairs state.groups)]
    (each [_ annotation (ipairs annotations)]
      (let [buffer-annotations (or (. state.buffers annotation.buf) [])]
        (table.insert buffer-annotations annotation)
        (set (. state.buffers annotation.buf) buffer-annotations)))))

(fn clear-group [group]
  (each [_ annotation (ipairs (or (. state.groups group) []))]
    (when (api.nvim_buf_is_valid annotation.buf)
      (each [_ mark (ipairs annotation.marks)]
        (pcall api.nvim_buf_del_extmark annotation.buf namespace mark))))
  (set (. state.groups group) nil)
  (rebuild-buffers)
  (pcall update-quickfix))

(fn clear []
  (each [buf _ (pairs state.buffers)]
    (when (api.nvim_buf_is_valid buf)
      (api.nvim_buf_clear_namespace buf namespace 0 -1)))
  (set state.buffers {})
  (set state.groups {})
  (pcall update-quickfix))

(fn forget-buffer [buf]
  (when (. state.buffers buf)
    (each [group annotations (pairs state.groups)]
      (for [index (length annotations) 1 -1]
        (when (= (. annotations index :buf) buf)
          (table.remove annotations index)))
      (when (= (length annotations) 0)
        (set (. state.groups group) nil)))
    (rebuild-buffers)
    (pcall update-quickfix)))

(local lifecycle-group (api.nvim_create_augroup :pi_annotation_lifecycle {:clear true}))
(api.nvim_create_autocmd :BufUnload
                         {:group lifecycle-group
                          :callback #(forget-buffer $1.buf)})
(api.nvim_create_autocmd :BufWipeout
                         {:group lifecycle-group
                          :callback
                          (fn [event]
                            (let [buf event.buf]
                              (vim.schedule
                               #(if (api.nvim_buf_is_valid buf)
                                    (when (. state.buffers buf)
                                      (pcall update-quickfix))
                                    (forget-buffer buf)))))})
(api.nvim_create_autocmd [:TextChanged :TextChangedI :TextChangedP]
                         {:group lifecycle-group
                          :callback #(when (. state.buffers $1.buf)
                                       (pcall update-quickfix))})

(fn display-width [buf text ?column]
  (api.nvim_buf_call buf #(vim.fn.strdisplaywidth text (or ?column 7))))

(fn wrap-prefix [buf text ?column]
  (let [prefix (or (text:match "^%s*[-+*]%s+%[[ xX-]%]%s+")
                   (text:match "^%s*[-+*]%s+")
                   (text:match "^%s*%d+[.)]%s+")
                   (text:match "^%s*")
                   "")]
    (values (string.rep " " (display-width buf prefix ?column)) (length prefix))))

(fn fit [buf text width ?column]
  (var fit 0)
  (var done? false)
  (let [characters (vim.str_utfindex text :utf-32)]
    (for [index 1 characters &until done?]
      (let [byte (vim.str_byteindex text :utf-32 index true)]
        (if (> (display-width buf (text:sub 1 byte) ?column) width)
            (set done? true)
            (set fit byte))))
    (if (and (= fit 0) (> characters 0))
        (vim.str_byteindex text :utf-32 1 true)
        fit)))

(fn wrap-line [buf text width ?column]
  (if (= text "")
      [""]
      (let [lines []
            (initial-continuation initial-protected) (wrap-prefix buf text ?column)]
        (var continuation initial-continuation)
        (var protected initial-protected)
        (var remaining text)
        (when (>= (display-width buf continuation ?column) width)
          (set continuation "")
          (set protected 0))
        (while (> (display-width buf remaining ?column) width)
          (let [fit-width (fit buf remaining width ?column)
                segment (remaining:sub 1 fit-width)]
            (var break-start nil)
            (var break-end nil)
            (var search 1)
            (var searching? true)
            (while searching?
              (let [(first last) (segment:find "%s+" search)]
                (if (not first)
                    (set searching? false)
                    (do
                      (when (> first protected)
                        (set break-start first)
                        (set break-end last))
                      (set search (+ last 1))))))
            (if break-start
                (let [rest (.. (segment:sub (+ break-end 1))
                               (remaining:sub (+ fit-width 1)))
                      before-break (segment:sub 1 (- break-start 1))]
                  (table.insert lines (pick-values 1 (before-break:gsub "%s+$" "")))
                  (set remaining (.. continuation (rest:gsub "^%s+" "")))
                  (set protected (length continuation)))
                (let [next-line (.. continuation (remaining:sub (+ fit-width 1)))]
                  (if (= next-line remaining)
                      (do
                        (set remaining (remaining:sub (+ fit-width 1)))
                        (set continuation "")
                        (set protected 0))
                      (do
                        (table.insert lines segment)
                        (set remaining next-line)
                        (set protected (length continuation))))))))
        (table.insert lines remaining)
        lines)))

(fn box [buf lines text-width ?connector source-border?]
  (let [box-column (if ?connector (+ ?connector 2) 5)
        text-column (+ box-column 2)
        content-limit (math.max 1 (- text-width text-column 2))
        wrapped []]
    (each [_ line (ipairs lines)]
      (vim.list_extend wrapped (wrap-line buf line content-limit text-column)))
    (var content-width 1)
    (each [_ line (ipairs wrapped)]
      (set content-width (math.max content-width (display-width buf line text-column))))
    (let [connector-prefix (string.rep " " (or ?connector 0))
          box-prefix (string.rep " " box-column)
          virtual-lines []]
      (when source-border?
        (let [border-width (math.max text-width vim.o.columns)]
          (table.insert virtual-lines
                        [[(.. (string.rep "─" ?connector)
                              "┬"
                              (string.rep "─" (- border-width ?connector 1)))
                          "FloatBorder"]])))
      (table.insert virtual-lines
                    [[(.. (if ?connector (.. connector-prefix "│ ┌") (.. box-prefix "┌"))
                          (string.rep "─" (+ content-width 2))
                          "┐")
                      "FloatBorder"]])
      (each [index line (ipairs wrapped)]
        (let [left (if (and ?connector (= index 1)) (.. connector-prefix "└─┤ ") (.. box-prefix "│ "))
              padding (string.rep " " (+ (- content-width (display-width buf line text-column)) 1))]
          (table.insert virtual-lines [[left "FloatBorder"] [line "NormalFloat"] [(.. padding "│") "FloatBorder"]])))
      (table.insert virtual-lines
                    [[(.. box-prefix "└" (string.rep "─" (+ content-width 2)) "┘") "FloatBorder"]])
      virtual-lines)))

(fn integer? [value minimum]
  (and (= (type value) :number) (= (% value 1) 0) (>= value minimum)))

(fn normal-file-buffer [value cwd]
  (assert (= (type value) :string) "Annotation file path must be a string")
  (let [value (if (= (value:sub 1 1) "@") (value:sub 2) value)
        path (vim.fs.abspath value {:cwd cwd})
        stat (vim.uv.fs_stat path)]
    (assert (and stat (= stat.type :file)) (.. "Neovim cannot find file: " path))
    (let [buf (vim.fn.bufadd path)]
      (vim.fn.bufload buf)
      (assert (and (api.nvim_buf_is_valid buf)
                   (= (. vim.bo buf :buftype) ""))
              "Neovim can only annotate normal file buffers")
      (when (not (. vim.bo buf :modified))
        (api.nvim_buf_call buf #(vim.cmd "silent checktime")))
      (values path buf))))

(fn byte-column [buf line character]
  (let [text (. (api.nvim_buf_get_lines buf (- line 1) line false) 1)
        (ok column) (pcall vim.str_byteindex text :utf-32 character true)]
    (assert ok (.. "Character offset is outside line " line))
    column))

(fn normal-window? [win]
  (and (= (type win) :number)
       (api.nvim_win_is_valid win)
       (= (. (api.nvim_win_get_config win) :relative) "")))

(fn text-width [buf ?presented]
  (var win (and ?presented ?presented.window))
  (when (not (normal-window? win))
    (set win nil)
    (each [_ candidate (ipairs (vim.fn.win_findbuf buf)) &until win]
      (when (normal-window? candidate)
        (set win candidate))))
  (if (not win)
      (math.max 10 (- (math.floor (/ vim.o.columns 2)) 2))
      (let [wininfo (. (vim.fn.getwininfo win) 1)]
        (math.max 10 (- (api.nvim_win_get_width win)
                        (or (and wininfo wininfo.textoff) 0)
                        2)))))

(fn position-before? [left right]
  (or (< (. left 1) (. right 1))
      (and (= (. left 1) (. right 1)) (< (. left 2) (. right 2)))))

(fn annotation-interval [annotation]
  (let [start (api.nvim_buf_get_extmark_by_id annotation.buf namespace annotation.mark {})
        finish (api.nvim_buf_get_extmark_by_id annotation.buf namespace annotation.finish_mark {})]
    (when (and (= (length start) 2) (= (length finish) 2))
      (values start (if (= annotation.kind :block) [(+ (. finish 1) 1) 0] finish)))))

(fn validate-disjoint [prepared]
  (let [by-buffer {}]
    (each [_ annotations (pairs state.groups)]
      (each [_ annotation (ipairs annotations)]
        (when (api.nvim_buf_is_valid annotation.buf)
          (let [(start finish) (annotation-interval annotation)]
            (when start
              (let [intervals (or (. by-buffer annotation.buf) [])]
                (table.insert intervals {:start start :finish finish})
                (set (. by-buffer annotation.buf) intervals)))))))
    (each [_ item (ipairs prepared)]
      (let [intervals (or (. by-buffer item.buf) [])]
        (table.insert intervals {:start item.interval_start :finish item.interval_finish})
        (set (. by-buffer item.buf) intervals)))
    (each [_ intervals (pairs by-buffer)]
      (table.sort intervals #(position-before? $1.start $2.start))
      (for [index 2 (length intervals)]
        (assert (not (position-before? (. intervals index :start) (. intervals (- index 1) :finish)))
                "Annotations must not overlap or contain one another")))))

(fn prepare [request cwd]
  (let [prepared []]
    (each [file-index file (ipairs request.annotations)]
      (assert (and (= (type file) :table)
                   (= (type file.filename) :string)
                   (vim.islist file.ranges))
              (.. "Invalid annotation file " file-index))
      (let [(path buf) (normal-file-buffer file.filename cwd)
            line-count (api.nvim_buf_line_count buf)]
        (assert (not (. vim.bo buf :modified))
                (.. "Cannot annotate a buffer with unsaved changes: " path))
        (each [range-index range (ipairs file.ranges)]
          (let [text-lines (and (= (type range) :table)
                                (= (type range.text) :string)
                                (vim.split range.text "\n" {:plain true}))]
            (assert (and text-lines
                         (not= range.text "")
                         (<= (length range.text) 16000)
                         (<= (length text-lines) 50))
                    (.. "Invalid annotation text in file " file-index " range " range-index))
            (let [block? (and (= range.kind :block)
                              (integer? range.startLine 1)
                              (integer? range.endLine 1)
                              (= range.line nil)
                              (= range.startCharacter nil)
                              (= range.endCharacter nil))
                  inline? (and (= range.kind :inline)
                               (integer? range.line 1)
                               (integer? range.startCharacter 0)
                               (integer? range.endCharacter 0)
                               (= range.startLine nil)
                               (= range.endLine nil))
                  item {:buf buf :path path :lines text-lines}]
              (assert (or block? inline?) "Range must be either a block or inline annotation")
              (if block?
                  (do
                    (assert (and (<= range.startLine range.endLine)
                                 (<= range.endLine line-count))
                            (.. "Invalid line range in file " file-index " range " range-index))
                    (let [last-line (. (api.nvim_buf_get_lines buf (- range.endLine 1) range.endLine false) 1)]
                      (set item.kind :block)
                      (set item.start [(- range.startLine 1) 0])
                      (set item.finish [(- range.endLine 1) (length last-line)])
                      (set item.interval_start item.start)
                      (set item.interval_finish [range.endLine 0])
                      (set item.present_line range.startLine)))
                  (do
                    (assert (and (<= range.line line-count)
                                 (< range.startCharacter range.endCharacter))
                            (.. "Invalid inline range in file " file-index " range " range-index))
                    (set item.kind :inline)
                    (set item.start [(- range.line 1) (byte-column buf range.line range.startCharacter)])
                    (set item.finish [(- range.line 1) (byte-column buf range.line range.endCharacter)])
                    (set item.interval_start item.start)
                    (set item.interval_finish item.finish)
                    (set item.present_line range.line)
                    (set item.present_character range.startCharacter)))
              (table.insert prepared item)
              (assert (<= (length prepared) 100) "A set action can add at most 100 annotations"))))))
    prepared))

(fn execute [request cwd]
  (assert (and (= (type request.namespace) :string) (not= request.namespace ""))
          "Annotation namespace must contain 1 to 100 characters")
  (assert (or (= request.action :set) (= request.action :clear))
          "Annotation action must be set or clear")
  (assert (vim.islist request.annotations) "Annotations must be an array")
  (if (= request.action :clear)
      (do
        (assert (= (length request.annotations) 0) "Clear action requires an empty annotations array")
        (let [count (length (or (. state.groups request.namespace) []))]
          (clear-group request.namespace)
          {:text (: "Cleared %d annotation(s) in %s." :format count request.namespace)
           :details {:namespace request.namespace :count count}}))
      (let [prepared (prepare request cwd)]
        (assert (> (length prepared) 0) "Set action requires at least one annotation")
        (validate-disjoint prepared)
        (let [first (. prepared 1)
              presented (if (not= request.present false)
                            (present.buffer first.buf
                                            {:line first.present_line
                                             :character first.present_character}))
              added []
              (ok err)
              (pcall
               #(each [_ item (ipairs prepared)]
                  (let [text-width (text-width item.buf presented)
                        border [[(string.rep "─" (math.max text-width vim.o.columns)) "FloatBorder"]]
                        marks []
                        mark (api.nvim_buf_set_extmark item.buf namespace (. item.start 1) (. item.start 2)
                                                       {:sign_text "●"
                                                        :sign_hl_group :PiAnnotationSign
                                                        :priority 200
                                                        :right_gravity false
                                                        :undo_restore false})
                        annotation {:buf item.buf :kind item.kind :mark mark :marks marks}]
                    (table.insert marks mark)
                    (table.insert added annotation)
                    (if (= item.kind :block)
                        (table.insert marks
                                      (api.nvim_buf_set_extmark item.buf namespace (. item.start 1) (. item.start 2)
                                                               {:virt_lines [border]
                                                                :virt_lines_above true
                                                                :virt_lines_overflow :trunc
                                                                :right_gravity false
                                                                :undo_restore false}))
                        (table.insert marks
                                      (api.nvim_buf_set_extmark item.buf namespace (. item.start 1) (. item.start 2)
                                                               {:end_row (. item.finish 1)
                                                                :end_col (. item.finish 2)
                                                                :hl_group :PiAnnotationUnderline
                                                                :right_gravity false
                                                                :end_right_gravity true
                                                                :priority 200
                                                                :undo_restore false})))
                    (let [finish-mark
                          (api.nvim_buf_set_extmark item.buf namespace (. item.finish 1) (. item.finish 2)
                                                   {:virt_lines (box item.buf
                                                                     item.lines
                                                                     text-width
                                                                     (if (= item.kind :block) 3)
                                                                     (= item.kind :block))
                                                    :virt_lines_overflow :trunc
                                                    :right_gravity true
                                                    :undo_restore false})]
                      (table.insert marks finish-mark)
                      (set annotation.finish_mark finish-mark)))))]
          (when (not ok)
            (each [_ annotation (ipairs added)]
              (each [_ mark (ipairs annotation.marks)]
                (pcall api.nvim_buf_del_extmark annotation.buf namespace mark))))
          (assert ok err)
          (let [group (or (. state.groups request.namespace) [])]
            (vim.list_extend group added)
            (set (. state.groups request.namespace) group))
          (rebuild-buffers)
          (let [(quickfix-ok quickfix-error) (pcall update-quickfix)]
            (when (not quickfix-ok)
              (vim.notify (.. "Could not update annotation quickfix list: " (tostring quickfix-error))
                          vim.log.levels.WARN
                          {:title :Pi})))
          {:text (: "Added %d annotation(s) in %s." :format (length added) request.namespace)
           :details {:namespace request.namespace :count (length added)}}))))

{:clear clear
 :execute execute}
