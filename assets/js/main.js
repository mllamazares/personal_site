document.addEventListener('DOMContentLoaded', () => {
    document.querySelectorAll('a[href^="http"]').forEach(a => a.setAttribute('target', '_blank'));

    // Heading anchor links (copy URL with #section-slug to clipboard)
    var linkIcon = '<svg width="16" height="16" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"><path d="M10 13a5 5 0 0 0 7.54.54l3-3a5 5 0 0 0-7.07-7.07l-1.72 1.71"/><path d="M14 11a5 5 0 0 0-7.54-.54l-3 3a5 5 0 0 0 7.07 7.07l1.71-1.71"/></svg>';
    var anchorCheckIcon = '<svg width="16" height="16" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"><polyline points="20 6 9 17 4 12"/></svg>';
    document.querySelectorAll('article h2[id], article h3[id], article h4[id], article h5[id], article h6[id]').forEach(function (h) {
        var anchor = document.createElement('a');
        anchor.className = 'heading-anchor';
        anchor.href = '#' + h.id;
        anchor.setAttribute('aria-label', 'copy link to this section');
        anchor.innerHTML = linkIcon;
        anchor.addEventListener('click', function (e) {
            e.preventDefault();
            var url = window.location.origin + window.location.pathname + '#' + h.id;
            history.replaceState(null, '', '#' + h.id);
            navigator.clipboard.writeText(url).then(function () {
                anchor.innerHTML = anchorCheckIcon;
                anchor.classList.add('copied');
                setTimeout(function () {
                    anchor.innerHTML = linkIcon;
                    anchor.classList.remove('copied');
                }, 1500);
            });
        });
        h.insertBefore(anchor, h.firstChild);
    });

    // Code block toolbar (language label + copy button)
    document.querySelectorAll('pre > code').forEach(function (code) {
        var pre = code.parentElement;
        var toolbar = document.createElement('div');
        toolbar.className = 'code-toolbar';

        // Language label
        var lang = '';
        var cls = code.className || '';
        var match = cls.match(/language-(\S+)/);
        if (match) lang = match[1];
        var label = document.createElement('span');
        label.className = 'code-lang';
        label.textContent = lang;
        toolbar.appendChild(label);

        // Copy button
        var copyIcon = '<svg width="14" height="14" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"><rect x="9" y="9" width="13" height="13" rx="2"/><path d="M5 15H4a2 2 0 0 1-2-2V4a2 2 0 0 1 2-2h9a2 2 0 0 1 2 2v1"/></svg>';
        var checkIcon = '<svg width="14" height="14" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"><polyline points="20 6 9 17 4 12"/></svg>';
        var btn = document.createElement('button');
        btn.className = 'copy-btn';
        btn.innerHTML = copyIcon;
        btn.addEventListener('click', function () {
            navigator.clipboard.writeText(code.textContent).then(function () {
                btn.innerHTML = checkIcon;
                btn.classList.add('copied');
                setTimeout(function () {
                    btn.innerHTML = copyIcon;
                    btn.classList.remove('copied');
                }, 1500);
            });
        });
        toolbar.appendChild(btn);

        pre.insertBefore(toolbar, code);
    });

    // Table of Contents Generation
    const tocSidebar = document.getElementById('toc-sidebar');
    if (tocSidebar) {
        const tocWrapper = tocSidebar.querySelector('.toc-wrapper');
        const article = document.querySelector('article');
        const headers = document.querySelectorAll('article h2, article h3');

        if (headers.length > 0 && tocWrapper && article) {
            const list = document.createElement('ul');

            const positionLinks = () => {
                const articleTop = article.offsetTop;
                const articleHeight = article.offsetHeight;
                const wrapperHeight = tocWrapper.offsetHeight;
                const minGap = 4;
                let lastVisualBottom = -Infinity;

                headers.forEach((header, index) => {
                    let li = list.children[index];
                    if (!li) {
                        li = document.createElement('li');
                        const a = document.createElement('a');
                        a.textContent = header.textContent;
                        a.href = `#${header.id}`;
                        a.setAttribute('data-target', header.id);
                        li.appendChild(a);
                        list.appendChild(li);
                    }

                    // Map header's position in the article to the sidebar's height
                    const relativeTop = header.offsetTop - articleTop;
                    const percentage = Math.max(0, Math.min(100, (relativeTop / articleHeight) * 100));
                    let topPx = (percentage / 100) * wrapperHeight;

                    // Prevent overlap (li uses translateY(-50%), so visual edges are offset)
                    const itemHeight = li.offsetHeight || 20;
                    const visualTop = topPx - itemHeight / 2;
                    if (visualTop < lastVisualBottom + minGap) {
                        topPx = lastVisualBottom + minGap + itemHeight / 2;
                    }

                    li.style.top = `${topPx}px`;
                    lastVisualBottom = topPx + itemHeight / 2;
                });
            };

            tocWrapper.appendChild(list);

            // Initial position
            // Wait for images/content to load to get correct heights
            window.addEventListener('load', positionLinks);
            window.addEventListener('resize', positionLinks);
            positionLinks(); // One immediate try

            // Active Section Highlighting
            const observerOptions = {
                root: null,
                rootMargin: '-10% 0px -80% 0px', // Trigger when section is near top
                threshold: 0
            };

            const observer = new IntersectionObserver((entries) => {
                entries.forEach(entry => {
                    if (entry.isIntersecting) {
                        const activeId = entry.target.id;
                        document.querySelectorAll('#toc-sidebar a').forEach(link => {
                            const isActive = link.getAttribute('data-target') === activeId;
                            link.classList.toggle('active', isActive);
                        });
                    }
                });
            }, observerOptions);

            headers.forEach(header => observer.observe(header));
        }
    }

    // Footnotes Sidebar Logic
    const footnoteSidebar = document.getElementById('footnote-sidebar');
    if (footnoteSidebar) {
        const references = document.querySelectorAll('sup[id^="fnref"]');
        const footnotesList = document.querySelectorAll('.footnotes ol li');
        const article = document.querySelector('article');

        if (references.length > 0 && footnotesList.length > 0) {
            // Move footnotes to sidebar
            references.forEach((ref, index) => {
                const originalFootnote = footnotesList[index];
                if (originalFootnote) {
                    const item = document.createElement('div');
                    item.className = 'footnote-item';
                    item.setAttribute('data-ref', ref.id);
                    // Clone content but strip back link
                    const content = originalFootnote.cloneNode(true);
                    const backLink = content.querySelector('.reversefootnote');
                    if (backLink) backLink.remove();

                    item.innerHTML = `<span class="footnote-ref">${index + 1}.</span> ${content.innerHTML}`;
                    footnoteSidebar.appendChild(item);

                    // Hover effects
                    const highlight = (active) => {
                        item.classList.toggle('active', active);
                        ref.closest('a') ? ref.closest('a').classList.toggle('active', active) : null;
                    };
                    item.addEventListener('mouseenter', () => highlight(true));
                    item.addEventListener('mouseleave', () => highlight(false));
                    ref.addEventListener('mouseenter', () => highlight(true));
                    ref.addEventListener('mouseleave', () => highlight(false));
                }
            });

            const positionFootnotes = () => {
                // Ensure sidebar matches article vertical position
                // We use absolute positioning relative to the document/parent
                const articleRect = article.getBoundingClientRect();
                const absoluteSidebarTop = articleRect.top + window.scrollY;

                footnoteSidebar.style.position = 'absolute';
                footnoteSidebar.style.top = `${absoluteSidebarTop}px`;
                footnoteSidebar.style.height = `${article.offsetHeight}px`;

                let lastBottom = 0;

                // We need to calculate positions. If the sidebar is hidden (display:none),
                // we cannot get offsetHeight of items.
                // However, on mobile it is hidden by CSS. We might not care about positioning then.
                // But for the calculation to work if we resize to desktop, we need it to run then.

                if (window.getComputedStyle(footnoteSidebar).display === 'none') return;

                const items = footnoteSidebar.children;
                if (items.length !== references.length) return; // mismatch safety

                references.forEach((ref, index) => {
                    const item = items[index];
                    const refRect = ref.getBoundingClientRect();
                    const refAbsoluteTop = refRect.top + window.scrollY;

                    // Calculate relative top within the sidebar
                    let targetTop = refAbsoluteTop - absoluteSidebarTop;

                    // Prevent Overlap
                    // If targetTop is higher (smaller number) than lastBottom, push it down
                    if (targetTop < lastBottom) {
                        targetTop = lastBottom + 10; // 10px buffer
                    }

                    item.style.top = `${targetTop}px`;

                    // Update lastBottom for next item
                    // Note: offsetHeight defaults to 0 if element is not displayed.
                    lastBottom = targetTop + item.offsetHeight;
                });
            };

            // Use ResizeObserver to handle layout changes (e.g., lazy loaded images)
            const resizeObserver = new ResizeObserver(() => {
                positionFootnotes();
            });
            resizeObserver.observe(article);

            // Also listen to window resize for horizontal layout changes
            window.addEventListener('resize', positionFootnotes);

            // Initial calculation
            positionFootnotes();
        }
    }

    // Text Scramble Effect
    if (window.baffle) {
        let b = baffle('h1', {
            characters: '!<>-_\\/[]{}*^?#&%$()________¶▒░▓',
            speed: 50
        });
        b.start();
        b.reveal(400, 400); // 400ms delay, 400ms duration
    }

});
