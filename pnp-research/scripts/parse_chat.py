#!/usr/bin/env python3
"""Parse a saved Claude.ai, ChatGPT, or Gemini conversation HTML into markdown.

Usage: python parse_chat.py <input.htm> <output.md> [--format claude|chatgpt|gemini]

If --format is omitted, auto-detects based on HTML content.
"""
import re
import os
import sys
from html.parser import HTMLParser
from collections import Counter

sys.stdout = open(sys.stdout.fileno(), mode='w', encoding='utf-8', buffering=1)


class HTMLToMarkdown(HTMLParser):
    """Convert HTML fragment to markdown text."""
    def __init__(self):
        super().__init__()
        self.output = []
        self.in_pre = False
        self.in_code = False
        self.in_strong = False
        self.in_em = False
        self.in_li = False
        self.in_h = 0
        self.list_type = []
        self.ol_counter = []
        self.skip_depth = 0
        self.in_a = False
        self.href = ''
        self.link_text = []
        self.code_lang = ''

    def handle_starttag(self, tag, attrs):
        attrs_d = dict(attrs)
        cls = attrs_d.get('class', '')
        testid = attrs_d.get('data-testid', '')

        if tag == 'svg' or tag == 'button':
            self.skip_depth += 1
            return
        if self.skip_depth > 0:
            if tag in ('div', 'span', 'svg', 'button', 'path', 'g', 'circle', 'rect', 'line', 'polyline', 'polygon'):
                self.skip_depth += 1
            return

        if testid in ('action-bar-copy', 'action-bar-retry', 'copy-turn-action-button',
                       'good-response-turn-action-button', 'bad-response-turn-action-button',
                       'project-save-turn-action-button'):
            self.skip_depth += 1
            return

        if tag == 'pre':
            self.in_pre = True
            lang = ''
            if 'language-' in cls:
                lang_m = re.search(r'language-(\w+)', cls)
                if lang_m:
                    lang = lang_m.group(1)
            self.code_lang = lang
            self.output.append(f'\n```{lang}\n')
        elif tag == 'code':
            if not self.in_pre:
                self.in_code = True
                self.output.append('`')
            else:
                if 'language-' in cls:
                    lang_m = re.search(r'language-(\w+)', cls)
                    if lang_m and not self.code_lang:
                        self.code_lang = lang_m.group(1)
                        for idx in range(len(self.output)-1, -1, -1):
                            if self.output[idx].startswith('\n```'):
                                self.output[idx] = f'\n```{self.code_lang}\n'
                                break
        elif tag in ('strong', 'b'):
            self.in_strong = True
            self.output.append('**')
        elif tag in ('em', 'i'):
            self.in_em = True
            self.output.append('*')
        elif tag in ('h1', 'h2', 'h3', 'h4', 'h5', 'h6'):
            level = int(tag[1])
            self.in_h = level
            # Bump heading levels by 2 to avoid conflicting with ## Human / ## Claude
            self.output.append('\n' + '#' * min(level + 2, 6) + ' ')
        elif tag == 'p':
            if not self.in_pre:
                self.output.append('\n\n')
        elif tag == 'br':
            self.output.append('\n')
        elif tag == 'ul':
            self.list_type.append('ul')
            self.output.append('\n')
        elif tag == 'ol':
            self.list_type.append('ol')
            self.ol_counter.append(0)
            self.output.append('\n')
        elif tag == 'li':
            self.in_li = True
            indent = '  ' * (len(self.list_type) - 1)
            if self.list_type and self.list_type[-1] == 'ol':
                self.ol_counter[-1] += 1
                self.output.append(f'{indent}{self.ol_counter[-1]}. ')
            else:
                self.output.append(f'{indent}- ')
        elif tag == 'a':
            self.in_a = True
            self.href = attrs_d.get('href', '')
            self.link_text = []
        elif tag == 'blockquote':
            self.output.append('\n> ')
        elif tag == 'hr':
            self.output.append('\n---\n')
        elif tag == 'sup':
            self.output.append('^(')
        elif tag == 'sub':
            self.output.append('~(')
        elif tag == 'table':
            self.output.append('\n')
        elif tag == 'tr':
            self.output.append('|')
        elif tag == 'th' or tag == 'td':
            self.output.append(' ')

    def handle_endtag(self, tag):
        if self.skip_depth > 0:
            if tag in ('div', 'span', 'svg', 'button', 'path', 'g', 'circle', 'rect', 'line', 'polyline', 'polygon'):
                self.skip_depth -= 1
            return

        if tag == 'pre':
            self.in_pre = False
            self.code_lang = ''
            self.output.append('\n```\n')
        elif tag == 'code':
            if not self.in_pre:
                self.in_code = False
                self.output.append('`')
        elif tag in ('strong', 'b'):
            self.in_strong = False
            self.output.append('**')
        elif tag in ('em', 'i'):
            self.in_em = False
            self.output.append('*')
        elif tag in ('h1', 'h2', 'h3', 'h4', 'h5', 'h6'):
            self.in_h = 0
            self.output.append('\n')
        elif tag == 'ul':
            if self.list_type:
                self.list_type.pop()
            self.output.append('\n')
        elif tag == 'ol':
            if self.list_type:
                self.list_type.pop()
            if self.ol_counter:
                self.ol_counter.pop()
            self.output.append('\n')
        elif tag == 'li':
            self.in_li = False
            self.output.append('\n')
        elif tag == 'a':
            self.in_a = False
            text = ''.join(self.link_text)
            if self.href and text:
                self.output.append(f'[{text}]({self.href})')
            elif text:
                self.output.append(text)
            self.link_text = []
        elif tag in ('sup', 'sub'):
            self.output.append(')')
        elif tag == 'th' or tag == 'td':
            self.output.append(' |')
        elif tag == 'tr':
            self.output.append('\n')
        elif tag == 'thead':
            # Add markdown table separator after header row
            self.output.append('|---|---|\n')

    def handle_data(self, data):
        if self.skip_depth > 0:
            return
        if self.in_a:
            self.link_text.append(data)
        else:
            self.output.append(data)

    def handle_entityref(self, name):
        entities = {'amp': '&', 'lt': '<', 'gt': '>', 'quot': '"', 'nbsp': ' '}
        self.handle_data(entities.get(name, f'&{name};'))

    def handle_charref(self, name):
        try:
            if name.startswith('x'):
                c = chr(int(name[1:], 16))
            else:
                c = chr(int(name))
            self.handle_data(c)
        except:
            self.handle_data(f'&#{name};')

    def get_markdown(self):
        text = ''.join(self.output)
        text = re.sub(r'\n{3,}', '\n\n', text)
        return text.strip()


def html_to_md(html_fragment):
    parser = HTMLToMarkdown()
    parser.feed(html_fragment)
    return parser.get_markdown()


def find_matching_close(html, start, tag='div'):
    """Find the closing tag that matches the opening tag at position start."""
    open_pat = re.compile(rf'<{tag}[\s>]')
    close_pat = re.compile(rf'</{tag}>')
    depth = 0
    i = start
    while i < len(html):
        open_m = open_pat.search(html, i)
        close_m = close_pat.search(html, i)
        if close_m is None:
            return len(html)
        if open_m and open_m.start() < close_m.start():
            depth += 1
            i = open_m.end()
        else:
            depth -= 1
            if depth == 0:
                return close_m.end()
            i = close_m.end()
    return len(html)


def detect_format(html):
    """Auto-detect whether HTML is from Claude, ChatGPT, or Gemini."""
    if 'data-message-author-role' in html:
        return 'chatgpt'
    if 'font-claude-response' in html:
        return 'claude'
    if 'model-response-text' in html and 'user-query-container' in html:
        return 'gemini'
    if 'claude.ai' in html[:5000]:
        return 'claude'
    if 'chatgpt' in html[:5000].lower() or 'openai' in html[:5000].lower():
        return 'chatgpt'
    if 'gemini' in html[:5000].lower():
        return 'gemini'
    return 'unknown'


def extract_title(html, fmt):
    """Extract conversation title from HTML."""
    m = re.search(r'<title>([^<]+)</title>', html)
    if m:
        title = m.group(1).strip()
        # Clean up common suffixes
        for suffix in [' - Claude', ' | ChatGPT', ' - ChatGPT', ' - Google Gemini', ' | Gemini']:
            if title.endswith(suffix):
                title = title[:-len(suffix)]
        return title
    return 'Conversation'


def parse_claude(html):
    """Parse Claude.ai saved HTML into turns."""
    turns = []

    for m in re.finditer(r'<div\s+data-testid="user-message"[^>]*>', html):
        div_start = m.start()
        div_end = find_matching_close(html, div_start)
        content = html[m.start():div_end]
        turns.append((div_start, 'Human', content))

    for m in re.finditer(r'<div\s+class="font-claude-response[^"]*"[^>]*>', html):
        div_start = m.start()
        div_end = find_matching_close(html, div_start)
        content = html[m.start():div_end]
        turns.append((div_start, 'Claude', content))

    turns.sort(key=lambda t: t[0])
    return turns


def parse_chatgpt(html):
    """Parse ChatGPT saved HTML into turns."""
    turns = []

    for m in re.finditer(r'data-message-author-role="(user|assistant)"', html):
        role = m.group(1)
        speaker = 'Human' if role == 'user' else 'ChatGPT'

        # Find the containing div with this attribute
        # Go back to find the opening <div
        line_start = html.rfind('<div', max(0, m.start() - 500), m.start())
        if line_start == -1:
            line_start = m.start()

        div_end = find_matching_close(html, line_start)
        content = html[line_start:div_end]

        # For user messages, extract from whitespace-pre-wrap div
        if role == 'user':
            wp_m = re.search(r'<div\s+class="whitespace-pre-wrap"[^>]*>', content)
            if wp_m:
                wp_start = wp_m.start()
                wp_end = find_matching_close(content, wp_start)
                content = content[wp_start:wp_end]

        # For assistant messages, extract from the markdown prose div
        if role == 'assistant':
            md_m = re.search(r'<div\s+class="markdown\s+prose[^"]*"[^>]*>', content)
            if md_m:
                md_start = md_m.start()
                md_end = find_matching_close(content, md_start)
                content = content[md_start:md_end]

        turns.append((m.start(), speaker, content))

    turns.sort(key=lambda t: t[0])
    return turns


def parse_gemini(html):
    """Parse Google Gemini saved HTML into turns."""
    turns = []

    # Gemini structure:
    # User queries: <div class="query-text ..."> containing <p class="query-text-line">
    # Also user queries with file uploads that have no text (just file previews)
    # Gemini responses: <div class="markdown markdown-main-panel ..."> inside
    #   <structured-content-container class="model-response-text ...">

    # Strategy: find all user queries and model responses by position, interleave.
    # User queries come in two flavors:
    #  1. query-text divs with text content
    #  2. file-only uploads (no query-text, just file-preview)
    # We'll extract from conversation-container boundaries.

    # Find conversation containers and extract user text from each
    for m in re.finditer(r'class="query-text\s+gds-body-l[^"]*"', html):
        # This is a query-text div; find its containing block and extract text
        div_start = html.rfind('<div', max(0, m.start() - 300), m.start())
        if div_start == -1:
            div_start = m.start()
        div_end = find_matching_close(html, div_start)
        content = html[div_start:div_end]
        turns.append((m.start(), 'Human', content))

    # Find Gemini responses: the markdown div inside model-response-text
    for m in re.finditer(r'class="markdown\s+markdown-main-panel[^"]*"', html):
        div_start = html.rfind('<div', max(0, m.start() - 300), m.start())
        if div_start == -1:
            div_start = m.start()
        div_end = find_matching_close(html, div_start)
        content = html[div_start:div_end]
        turns.append((m.start(), 'Gemini', content))

    turns.sort(key=lambda t: t[0])
    return turns


def clean_thinking_artifacts(text, fmt):
    """Remove thinking/summary artifacts from AI responses."""
    noise_patterns = []

    if fmt == 'claude':
        noise_patterns = [
            r'^(?:Synthesized|Orchestrated|Architected|Charted|Incorporated|Analyzed|Evaluated|Prepared|Identified|Examined|Constructed|Mapped|Designed|Distilled|Extracted|Formulated|Investigated|Validated|Computed|Derived|Assessed|Compiled|Summarized|Verified|Cataloged|Outlined|Processed|Reviewed|Scanned|Surveyed|Traced|Recognized|Contemplated|Reflected|Acknowledged|Determined|Noted|Observed|Considered)[^\n]{10,120}\n',
            r'(?:The\s*\n?\s*)?user prompt is empty[^\n]*\n',
            r"I cannot provide a summary because[^\n]*\n",
            r"However, following the instruction to write the summary[^\n]*\n(?:of the original[^\n]*\n)?",
            r"However, I'll provide a summary[^\n]*\n",
            r"user prompt is empty, so there is no primary language[^\n]*\n",
            r"However, based on the thinking block[^\n]*\n",
            r"user prompt is empty, so I cannot determine[^\n]*\n",
            r'^\s*analysis\s*$',
            r'Now I have the full picture[^\n]*\n',
            r'(?:Document|Code|Artifact)\s*\xb7\s*(?:MD|HTML|JS|CSS|Python|Haskell)\s*',
            r'\nP = NP\s*$',
        ]

    if fmt == 'gemini':
        noise_patterns = [
            # "You said" prefix from Gemini's screen-reader h2 leaking into query text
            r'^You said\s*\n?',
            # " Gemini said " prefix
            r'^Gemini said\s*\n?',
        ]

    for pattern in noise_patterns:
        text = re.sub(pattern, '', text, flags=re.MULTILINE)

    return text


def merge_consecutive(turns):
    """Merge consecutive turns from the same speaker."""
    merged = []
    for speaker, content in turns:
        if merged and merged[-1][0] == speaker:
            prev_speaker, prev_content = merged[-1]
            merged[-1] = (prev_speaker, prev_content + '\n\n' + content)
        else:
            merged.append((speaker, content))
    return merged


def main():
    if len(sys.argv) < 3:
        print(f"Usage: {sys.argv[0]} <input.htm> <output.md> [--format claude|chatgpt]")
        sys.exit(1)

    input_path = sys.argv[1]
    output_path = sys.argv[2]

    forced_fmt = None
    if '--format' in sys.argv:
        idx = sys.argv.index('--format')
        if idx + 1 < len(sys.argv):
            forced_fmt = sys.argv[idx + 1]

    with open(input_path, 'r', encoding='utf-8') as f:
        html = f.read()

    fmt = forced_fmt or detect_format(html)
    print(f"Detected format: {fmt}")
    print(f"File size: {len(html)} chars")

    title = extract_title(html, fmt)
    print(f"Title: {title}")

    # Parse turns
    if fmt == 'claude':
        raw_turns = parse_claude(html)
    elif fmt == 'chatgpt':
        raw_turns = parse_chatgpt(html)
    elif fmt == 'gemini':
        raw_turns = parse_gemini(html)
    else:
        print(f"Unknown format: {fmt}")
        sys.exit(1)

    print(f"Raw turns: {len(raw_turns)}")

    # Convert HTML to markdown and clean up
    ai_names = {'claude': 'Claude', 'chatgpt': 'ChatGPT', 'gemini': 'Gemini'}
    ai_name = ai_names.get(fmt, 'AI')
    converted = []
    for pos, speaker, content in raw_turns:
        md = html_to_md(content)
        md = clean_thinking_artifacts(md, fmt)
        md = md.strip()
        if md:
            converted.append((speaker, md))

    # Merge consecutive same-speaker turns
    merged = merge_consecutive(converted)

    human_count = sum(1 for s, _ in merged if s == 'Human')
    ai_count = sum(1 for s, _ in merged if s != 'Human')
    print(f"After merge: {len(merged)} turns ({human_count} Human, {ai_count} {ai_name})")

    # Fix mid-sentence line breaks from HTML rendering
    def fix_linebreaks(text):
        text = re.sub(r'\n (\w)', lambda m: ' ' + m.group(1), text)
        text = re.sub(r'\n{3,}', '\n\n', text)
        return text

    # Build output
    md_parts = [f'# {title}\n']
    md_parts.append(f'*Conversation between a human and {ai_name}*\n\n---\n')

    for speaker, content in merged:
        md_parts.append(f'\n## {speaker}\n\n')
        md_parts.append(fix_linebreaks(content))
        md_parts.append('\n\n---\n')

    output_text = ''.join(md_parts)

    with open(output_path, 'w', encoding='utf-8') as f:
        f.write(output_text)

    lines = output_text.split('\n')
    print(f"\nWrote {len(output_text)} chars, {len(lines)} lines to {output_path}")

    # Preview
    print("\nPreview (first 30 lines):")
    for line in lines[:30]:
        print(line)


if __name__ == '__main__':
    main()
