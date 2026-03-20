document.addEventListener('DOMContentLoaded', () => {
    document.querySelectorAll('a[href^="http"]').forEach(a => a.setAttribute('target', '_blank'));

    // Table of Contents Generation
    const tocSidebar = document.getElementById('toc-sidebar');
    if (tocSidebar) {
        const tocWrapper = tocSidebar.querySelector('.toc-wrapper');
        const article = document.querySelector('article');
        const headers = document.querySelectorAll('article h2, article h3');

        if (headers.length > 0 && tocWrapper && article) {
            const list = document.createElement('ul');

            const positionLinks = () => {
                const articleRect = article.getBoundingClientRect();
                const articleTop = article.offsetTop;
                const articleHeight = article.offsetHeight;

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

                    // Calculate relative position
                    // We map the header's position in the article to the sidebar's height
                    const relativeTop = header.offsetTop - articleTop;
                    const percentage = (relativeTop / articleHeight) * 100;

                    // Clamp between 0 and 100
                    const clampedDetails = Math.max(0, Math.min(100, percentage));
                    li.style.top = `${clampedDetails}%`;
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
    // Scroll to Top Arrow Logic
    const scrollToTopBtn = document.getElementById('scroll-to-top');

    if (scrollToTopBtn) {
        window.addEventListener('scroll', () => {
            if (window.scrollY > 300) {
                scrollToTopBtn.classList.add('show');
            } else {
                scrollToTopBtn.classList.remove('show');
            }
        });

        scrollToTopBtn.addEventListener('click', () => {
            window.scrollTo({
                top: 0,
                behavior: 'smooth'
            });
        });
    }
});
