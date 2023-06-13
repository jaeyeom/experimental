"""html_convert converts span tag to code tags if the style is in monospace fonts.

The code can be used to convert Google Docs to Emacs Org or Markdown modes. The
code was written by ChatGPT.
"""

from bs4 import BeautifulSoup
import cssutils


def parse_styles(styles):
    style_sheet = cssutils.parseString(styles)
    monospace_classes = set()

    # List of acceptable monospace fonts
    monospace_fonts = ["monospace", "Roboto Mono", "Droid Sans Mono", "Courier New"]

    for rule in style_sheet:
        if rule.type == rule.STYLE_RULE:
            style = rule.style
            if 'font-family' in style and any(font in style['font-family'] for font in monospace_fonts):
                # Remove leading dot
                class_name = rule.selectorText.replace(".", "")
                monospace_classes.add(class_name)

    return monospace_classes


def convert_span_to_code(html_content):
    soup = BeautifulSoup(html_content, 'html.parser')

    styles = soup.find('style')
    monospace_classes = parse_styles(styles.string) if styles else set()

    # Find all span tags with monospace font classes
    for span in soup.find_all('span', class_=lambda value: value in monospace_classes if value else False):
        # Create a new code tag
        new_tag = soup.new_tag("code")
        # Copy the contents of the span tag to the code tag
        if span.string:
            new_tag.string = span.string.replace(u'\xa0', u' ')
        # Replace the span tag with the code tag
        span.replace_with(new_tag)

    # remove non-breaking spaces from whole document
    html_content = str(soup).replace(u'\xa0', u' ')

    return html_content

# You can use this function like so:
html_content = '''<html>
    <head>
        <style>
            .code { font-family: Roboto Mono; }
        </style>
    </head>
    <body>
        <p><span class="code">This is a code snippet</span></p>
    </body>
</html>'''

new_html_content = convert_span_to_code(html_content)

print(new_html_content)
