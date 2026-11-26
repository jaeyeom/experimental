package jira

import (
	"bytes"
	"strings"

	"github.com/yuin/goldmark"
	"github.com/yuin/goldmark/ast"
	"github.com/yuin/goldmark/text"
	"github.com/yuin/goldmark/util"
)

// adfDoc is an Atlassian document of version 1.
type adfDoc struct {
	Type    string     `json:"type"`
	Version int        `json:"version"`
	Content []*adfNode `json:"content"`
}

// adfNode is one ADF block or inline node.
type adfNode struct {
	Type    string         `json:"type"`
	Attrs   map[string]any `json:"attrs,omitempty"`
	Content []*adfNode     `json:"content,omitempty"`
	Text    string         `json:"text,omitempty"`
	Marks   []adfMark      `json:"marks,omitempty"`
}

// adfMark is a text mark such as strong, em, code, or link.
type adfMark struct {
	Type  string         `json:"type"`
	Attrs map[string]any `json:"attrs,omitempty"`
}

// adfDescription converts a Markdown body to an ADF doc.
// Headings, lists, fenced and indented code, block quotes, rules, inline
// code, emphasis, and links map to their ADF nodes. A single newline inside
// a paragraph is a hardBreak. Anything else, such as a table or raw HTML,
// keeps its original text. A blank body is nil.
func adfDescription(body string) *adfDoc {
	body = strings.ReplaceAll(body, "\r\n", "\n")
	body = strings.ReplaceAll(body, "\r", "\n")
	if strings.TrimSpace(body) == "" {
		return nil
	}
	src := []byte(body)
	root := goldmark.New().Parser().Parse(text.NewReader(src))
	conv := adfConverter{src: src}
	content := conv.blocks(root)
	if len(content) == 0 {
		return nil
	}
	return &adfDoc{Type: "doc", Version: 1, Content: content}
}

type adfConverter struct {
	src []byte
}

func (c adfConverter) blocks(parent ast.Node) []*adfNode {
	var out []*adfNode
	for n := parent.FirstChild(); n != nil; n = n.NextSibling() {
		if node := c.block(n); node != nil {
			out = append(out, node)
		}
	}
	return out
}

func (c adfConverter) block(n ast.Node) *adfNode {
	switch n := n.(type) {
	case *ast.Heading:
		return &adfNode{Type: "heading", Attrs: map[string]any{"level": n.Level}, Content: c.inlines(n, nil)}
	case *ast.Paragraph, *ast.TextBlock:
		return paragraph(c.inlines(n, nil))
	case *ast.List:
		return c.list(n)
	case *ast.FencedCodeBlock:
		node := codeBlock(c.lines(n))
		if lang := n.Language(c.src); len(lang) > 0 {
			node.Attrs = map[string]any{"language": string(lang)}
		}
		return node
	case *ast.CodeBlock:
		return codeBlock(c.lines(n))
	case *ast.Blockquote:
		return &adfNode{Type: "blockquote", Content: c.blocks(n)}
	case *ast.ThematicBreak:
		return &adfNode{Type: "rule"}
	default:
		return c.plain(n)
	}
}

func (c adfConverter) list(n *ast.List) *adfNode {
	node := &adfNode{Type: "bulletList"}
	if n.IsOrdered() {
		node.Type = "orderedList"
		if n.Start != 1 {
			node.Attrs = map[string]any{"order": n.Start}
		}
	}
	for item := n.FirstChild(); item != nil; item = item.NextSibling() {
		content := c.blocks(item)
		if len(content) == 0 {
			content = []*adfNode{paragraph(nil)}
		}
		node.Content = append(node.Content, &adfNode{Type: "listItem", Content: content})
	}
	return node
}

// plain keeps an unsupported block, such as raw HTML, as a paragraph of its
// original lines.
func (c adfConverter) plain(n ast.Node) *adfNode {
	raw := strings.TrimRight(c.lines(n), "\n")
	if raw == "" {
		return nil
	}
	var content []*adfNode
	for i, line := range strings.Split(raw, "\n") {
		if i > 0 {
			content = append(content, &adfNode{Type: "hardBreak"})
		}
		if line != "" {
			content = append(content, &adfNode{Type: "text", Text: line})
		}
	}
	return paragraph(content)
}

func (c adfConverter) lines(n ast.Node) string {
	var b bytes.Buffer
	lines := n.Lines()
	for i := 0; i < lines.Len(); i++ {
		seg := lines.At(i)
		b.Write(seg.Value(c.src))
	}
	return b.String()
}

func (c adfConverter) inlines(parent ast.Node, marks []adfMark) []*adfNode {
	var out []*adfNode
	for n := parent.FirstChild(); n != nil; n = n.NextSibling() {
		out = append(out, c.inline(n, marks)...)
	}
	return mergeText(out)
}

func (c adfConverter) inline(n ast.Node, marks []adfMark) []*adfNode {
	switch n := n.(type) {
	case *ast.Text:
		raw := n.Segment.Value(c.src)
		value := string(util.ResolveEntityNames(util.ResolveNumericReferences(util.UnescapePunctuations(raw))))
		out := textNodes(value, marks)
		if n.SoftLineBreak() || n.HardLineBreak() {
			out = append(out, &adfNode{Type: "hardBreak"})
		}
		return out
	case *ast.String:
		return textNodes(string(n.Value), marks)
	case *ast.CodeSpan:
		return textNodes(c.rawText(n), codeMarks(marks))
	case *ast.Emphasis:
		mark := adfMark{Type: "em"}
		if n.Level >= 2 {
			mark.Type = "strong"
		}
		return c.inlines(n, withMark(marks, mark))
	case *ast.Link:
		return c.inlines(n, withMark(marks, linkMark(string(n.Destination), string(n.Title))))
	case *ast.AutoLink, *ast.Image, *ast.RawHTML:
		return c.leaf(n, marks)
	default:
		if n.HasChildren() {
			return c.inlines(n, marks)
		}
		return nil
	}
}

// leaf converts an autolink, image, or raw HTML span to text.
func (c adfConverter) leaf(n ast.Node, marks []adfMark) []*adfNode {
	switch n := n.(type) {
	case *ast.AutoLink:
		label := string(n.Label(c.src))
		href := string(n.URL(c.src))
		if n.AutoLinkType == ast.AutoLinkEmail && !strings.HasPrefix(href, "mailto:") {
			href = "mailto:" + href
		}
		return textNodes(label, withMark(marks, linkMark(href, "")))
	case *ast.Image:
		alt := c.rawText(n)
		if alt == "" {
			alt = string(n.Destination)
		}
		return textNodes(alt, withMark(marks, linkMark(string(n.Destination), string(n.Title))))
	case *ast.RawHTML:
		var b strings.Builder
		for i := 0; i < n.Segments.Len(); i++ {
			seg := n.Segments.At(i)
			b.Write(seg.Value(c.src))
		}
		return textNodes(b.String(), marks)
	default:
		return nil
	}
}

// rawText is the source text of n's descendants, with no escape handling.
func (c adfConverter) rawText(n ast.Node) string {
	var b strings.Builder
	for child := n.FirstChild(); child != nil; child = child.NextSibling() {
		switch child := child.(type) {
		case *ast.Text:
			b.Write(child.Segment.Value(c.src))
		case *ast.String:
			b.Write(child.Value)
		default:
			b.WriteString(c.rawText(child))
		}
	}
	return b.String()
}

func paragraph(content []*adfNode) *adfNode {
	return &adfNode{Type: "paragraph", Content: content}
}

func codeBlock(code string) *adfNode {
	node := &adfNode{Type: "codeBlock"}
	code = strings.TrimSuffix(code, "\n")
	if code != "" {
		node.Content = []*adfNode{{Type: "text", Text: code}}
	}
	return node
}

func textNodes(value string, marks []adfMark) []*adfNode {
	if value == "" {
		return nil
	}
	return []*adfNode{{Type: "text", Text: value, Marks: marks}}
}

func linkMark(href, title string) adfMark {
	attrs := map[string]any{"href": href}
	if title != "" {
		attrs["title"] = title
	}
	return adfMark{Type: "link", Attrs: attrs}
}

// withMark returns a new mark list. The caller's slice is not changed.
func withMark(marks []adfMark, mark adfMark) []adfMark {
	out := make([]adfMark, 0, len(marks)+1)
	out = append(out, marks...)
	return append(out, mark)
}

// codeMarks keeps only a link next to code, because ADF allows no other
// mark with code.
func codeMarks(marks []adfMark) []adfMark {
	out := []adfMark{}
	for _, mark := range marks {
		if mark.Type == "link" {
			out = append(out, mark)
		}
	}
	return append(out, adfMark{Type: "code"})
}

// mergeText joins adjacent text nodes with the same marks.
func mergeText(nodes []*adfNode) []*adfNode {
	var out []*adfNode
	for _, n := range nodes {
		if len(out) > 0 {
			last := out[len(out)-1]
			if last.Type == "text" && n.Type == "text" && sameMarks(last.Marks, n.Marks) {
				last.Text += n.Text
				continue
			}
		}
		out = append(out, n)
	}
	return out
}

func sameMarks(a, b []adfMark) bool {
	if len(a) != len(b) {
		return false
	}
	for i := range a {
		if a[i].Type != b[i].Type || len(a[i].Attrs) != len(b[i].Attrs) {
			return false
		}
		for k, v := range a[i].Attrs {
			if b[i].Attrs[k] != v {
				return false
			}
		}
	}
	return true
}

// adfPlainText extracts plain text from an Atlassian Document Format document.
func adfPlainText(doc *adfDoc) string {
	if doc == nil || len(doc.Content) == 0 {
		return ""
	}
	return joinADFBlocks(doc.Content)
}

func joinADFBlocks(nodes []*adfNode) string {
	parts := make([]string, 0, len(nodes))
	for _, n := range nodes {
		if n == nil {
			continue
		}
		if text := adfNodeText(n); text != "" {
			parts = append(parts, text)
		}
	}
	return strings.Join(parts, "\n")
}

func adfNodeText(n *adfNode) string {
	switch n.Type {
	case "text":
		return n.Text
	case "hardBreak":
		return "\n"
	case "mention":
		if s, ok := n.Attrs["text"].(string); ok {
			return s
		}
		return ""
	case "emoji":
		if s, ok := n.Attrs["text"].(string); ok && s != "" {
			return s
		}
		if s, ok := n.Attrs["shortName"].(string); ok {
			return s
		}
		return ""
	case "blockquote", "bulletList", "orderedList", "listItem", "panel", "doc":
		return joinADFBlocks(n.Content)
	default:
		var b strings.Builder
		for _, child := range n.Content {
			if child == nil {
				continue
			}
			b.WriteString(adfNodeText(child))
		}
		return b.String()
	}
}
