<script lang="ts">
    import { ArrowLeft, ArrowRight, ExternalLink, Info, X } from "@lucide/svelte";
    import type { TextileGalleryItem, TextileMedia } from "$lib/data/original-textile-media";
    import { assetPath } from "$lib/utils/asset-path";
    import { exportFilename } from "$lib/utils/download";
    import { imageSize } from "$lib/utils/image-size";
    import { waterfall } from "$lib/utils/waterfall";

    let { media, textileName }: { media: TextileMedia; textileName: string } = $props();

    let selected = $state<TextileGalleryItem | null>(null);
    let activeAnnotations = $state<Record<string, number | undefined>>({});
    const annotationId = $props.id();
    function toggleAnnotation(image: string, number: number) {
        activeAnnotations[image] = activeAnnotations[image] === number ? undefined : number;
    }
    function revealAnnotation(image: string, number: number) {
        toggleAnnotation(image, number);
        if (activeAnnotations[image] !== number) return;
        requestAnimationFrame(() => {
            const target = document.getElementById(
                annotationId +
                    "-region-" +
                    media.stories.findIndex((story) => story.image === image) +
                    "-" +
                    number,
            );
            if (!target) return;
            const box = target.getBoundingClientRect();
            if (box.top < 100 || box.bottom > window.innerHeight - 24)
                target.scrollIntoView({
                    block: "center",
                    behavior: window.matchMedia("(prefers-reduced-motion: reduce)").matches
                        ? "instant"
                        : "smooth",
                });
        });
    }
    const selectedIndex = $derived(
        media.gallery.findIndex(
            (item) => item.image === selected?.image && item.title === selected?.title,
        ),
    );

    function stepImage(direction: -1 | 1) {
        const next = selectedIndex + direction;
        if (next >= 0 && next < media.gallery.length) selected = media.gallery[next];
    }

    function galleryKeydown(event: KeyboardEvent) {
        if (event.key === "ArrowLeft" || event.key === "ArrowRight") {
            event.preventDefault();
            stepImage(event.key === "ArrowLeft" ? -1 : 1);
        }
    }

    function modal(node: HTMLDialogElement) {
        const previousOverflow = document.body.style.overflow;
        const trigger =
            document.activeElement instanceof HTMLElement ? document.activeElement : null;
        node.showModal();
        document.body.style.overflow = "hidden";
        function trapFocus(event: KeyboardEvent) {
            if (event.key !== "Tab") return;
            const controls = Array.from(
                node.querySelectorAll<HTMLElement>(
                    'a[href], button:not([disabled]), input:not([disabled]), select:not([disabled]), textarea:not([disabled]), [tabindex]:not([tabindex="-1"])',
                ),
            ).filter((element) => element.getClientRects().length > 0);
            const first = controls[0];
            const last = controls[controls.length - 1];
            if (!first) {
                event.preventDefault();
                return;
            }
            if (event.shiftKey && document.activeElement === first) {
                event.preventDefault();
                last.focus();
            } else if (!event.shiftKey && document.activeElement === last) {
                event.preventDefault();
                first.focus();
            }
        }
        node.addEventListener("keydown", trapFocus);
        return {
            destroy() {
                node.removeEventListener("keydown", trapFocus);
                node.close();
                document.body.style.overflow = previousOverflow;
                trigger?.focus({ preventScroll: true });
            },
        };
    }
</script>

<section class="visual-evidence" aria-label={textileName}>
    <div class="page-shell evidence-grid">
        <header class="evidence-heading">
            <h2>Visual evidence</h2>
        </header>

        <div class="stories">
            {#each media.stories as story, storyIndex}
                <article class="story">
                    <figure
                        style={`--ratio: ${story.width} / ${story.height}; --image-ratio: ${story.width / story.height}`}
                    >
                        <img
                            src={assetPath(story.image)}
                            alt={story.title}
                            loading="lazy"
                            decoding="async"
                        />
                        {#each story.annotations as annotation}
                            <button
                                type="button"
                                class="annotation-box"
                                id={annotationId +
                                    "-region-" +
                                    storyIndex +
                                    "-" +
                                    annotation.number}
                                class:active={activeAnnotations[story.image] === annotation.number}
                                aria-label={`Annotation ${annotation.number}`}
                                aria-pressed={activeAnnotations[story.image] === annotation.number}
                                aria-describedby={`${annotationId}-${storyIndex}-${annotation.number}`}
                                onclick={() => toggleAnnotation(story.image, annotation.number)}
                                style={`--left: ${annotation.left}%; --top: ${annotation.top}%; --width: ${annotation.width}%; --height: ${annotation.height}%`}
                            >
                                <i>{annotation.number}</i>
                            </button>
                        {/each}
                    </figure>

                    <div class="story-copy">
                        <h3>{story.title}</h3>
                        {#if story.caption}<p class="caption">{story.caption}</p>{/if}
                        {#if story.creator || story.date || story.credit}
                            <p class="story-meta">
                                {[story.creator, story.date, story.credit]
                                    .filter(Boolean)
                                    .join(" · ")}
                            </p>
                        {/if}
                        <ol>
                            {#each story.annotations as annotation}
                                <li>
                                    <button
                                        type="button"
                                        class:active={activeAnnotations[story.image] ===
                                            annotation.number}
                                        aria-pressed={activeAnnotations[story.image] ===
                                            annotation.number}
                                        onclick={() =>
                                            revealAnnotation(story.image, annotation.number)}
                                    >
                                        <span>{annotation.number}</span>
                                        <p
                                            id={`${annotationId}-${storyIndex}-${annotation.number}`}
                                        >
                                            {annotation.text}
                                        </p>
                                    </button>
                                </li>
                            {/each}
                        </ol>
                        {#if story.catalogueUrl}
                            <a href={story.catalogueUrl} target="_blank" rel="noreferrer">
                                Collection record <ExternalLink size={12} />
                            </a>
                        {/if}
                    </div>
                </article>
            {/each}
        </div>

        <div class="gallery-heading">
            <h3>Related works <span>{media.gallery.length}</span></h3>
        </div>

        <div class="media-gallery" use:waterfall>
            {#each media.gallery as item}
                <article>
                    <button
                        type="button"
                        aria-label={item.title}
                        aria-haspopup="dialog"
                        onclick={() => (selected = item)}
                    >
                        <img
                            {...imageSize(item.image)}
                            src={assetPath(item.image)}
                            alt={item.title}
                            loading="lazy"
                            decoding="async"
                        />
                        <span><Info size={14} /> View details</span>
                    </button>
                    <div>
                        <h4>{item.title}</h4>
                        {#if item.date}<span>{item.date}</span>{/if}
                    </div>
                </article>
            {/each}
        </div>
    </div>
</section>

{#if selected}
    <dialog
        use:modal
        class="media-modal-backdrop"
        aria-labelledby="media-title"
        oncancel={() => (selected = null)}
        onkeydown={galleryKeydown}
        onclick={(event) => {
            if (event.currentTarget === event.target) selected = null;
        }}
    >
        <div class="media-modal">
            <button
                type="button"
                aria-label="Close image details"
                onclick={() => (selected = null)}
            >
                <X size={20} />
            </button>
            <figure>
                <img src={assetPath(selected.image)} alt={selected.title} />
                <nav class="image-navigation" aria-label="Gallery images">
                    <button
                        type="button"
                        aria-label="Previous image"
                        disabled={selectedIndex === 0}
                        onclick={() => stepImage(-1)}><ArrowLeft size={18} /></button
                    >
                    <span>{selectedIndex + 1} / {media.gallery.length}</span>
                    <button
                        type="button"
                        aria-label="Next image"
                        disabled={selectedIndex === media.gallery.length - 1}
                        onclick={() => stepImage(1)}><ArrowRight size={18} /></button
                    >
                </nav>
            </figure>
            <div>
                <p class="eyebrow">{selected.type || "Visual record"}</p>
                <h2 id="media-title" aria-live="polite">{selected.title}</h2>
                {#if selected.caption}<p class="modal-caption">{selected.caption}</p>{/if}
                <dl>
                    {#if selected.creator}<div>
                            <dt>Maker</dt>
                            <dd>{selected.creator}</dd>
                        </div>{/if}
                    {#if selected.date}<div>
                            <dt>Date</dt>
                            <dd>{selected.date}</dd>
                        </div>{/if}
                    {#if selected.credit}<div>
                            <dt>Collection</dt>
                            <dd>{selected.credit}</dd>
                        </div>{/if}
                </dl>
                <a
                    class="button"
                    href={assetPath(selected.image)}
                    download={exportFilename(
                        "textile",
                        [textileName, selected.title],
                        selected.image.split(".").at(-1) || "jpg",
                    )}>Download image</a
                >
                {#if selected.catalogueUrl}
                    <a class="button" href={selected.catalogueUrl} target="_blank" rel="noreferrer">
                        Collection record <ExternalLink size={14} />
                    </a>
                {/if}
            </div>
        </div>
    </dialog>
{/if}

<style>
    .visual-evidence {
        padding: clamp(4rem, 8vw, 8rem) 0;
        border-top: 1px solid var(--line-strong);
        color: var(--ink);
        background: var(--paper);
    }
    .evidence-grid {
        display: block;
    }
    .evidence-heading {
        margin-bottom: clamp(3rem, 6vw, 6rem);
    }
    .evidence-heading h2 {
        margin: 0;
        font: 400 clamp(3.6rem, 8vw, 9rem)/0.97 var(--editorial-font);
        letter-spacing: var(--display-tracking, -0.03em);
    }
    .stories {
        display: grid;
        gap: clamp(5rem, 10vw, 10rem);
    }
    .story {
        display: grid;
        grid-template-columns: minmax(0, 1.7fr) minmax(17rem, 0.65fr);
        gap: clamp(2rem, 5vw, 6rem);
        align-items: start;
    }
    .story:nth-child(even) {
        grid-template-columns: minmax(17rem, 0.65fr) minmax(0, 1.7fr);
    }
    .story:nth-child(even) figure {
        grid-column: 2;
    }
    .story:nth-child(even) .story-copy {
        grid-column: 1;
        grid-row: 1;
    }
    .story figure {
        position: relative;
        aspect-ratio: var(--ratio);
        margin: 0;
        overflow: hidden;
        background: #ebe7de;
    }
    .story figure > img {
        display: block;
        width: 100%;
        height: 100%;
        object-fit: contain;
    }
    .annotation-box {
        padding: 0;
        background: transparent;
        cursor: pointer;
        position: absolute;
        top: var(--top);
        left: var(--left);
        width: var(--width);
        height: var(--height);
        border: 2px solid #f7f5ef;
        box-shadow: 0 0 0 1px #17171170;
    }
    .annotation-box.active {
        z-index: 2;
        border-color: #f7f5ef;
        box-shadow: 0 0 0 100vmax #10110f80;
    }
    .annotation-box:hover,
    .annotation-box:focus-visible {
        border-color: #efbd7c;
    }
    .annotation-box i {
        position: absolute;
        top: -0.7rem;
        left: -0.7rem;
        display: grid;
        place-items: center;
        width: 1.5rem;
        height: 1.5rem;
        color: #171711;
        border-radius: 50%;
        background: #f7f5ef;
        font: 600 0.75rem var(--sans);
    }
    .story-copy {
        position: sticky;
        top: 7rem;
        padding-top: 1rem;
        border-top: 1px solid var(--ink);
    }
    .story-copy h3 {
        margin: 0 0 1.25rem;
        font: 400 clamp(1.9rem, 2.7vw, 3.2rem)/1.12 var(--editorial-font);
        letter-spacing: var(--display-tracking, -0.03em);
    }
    .caption {
        margin-bottom: 1.2rem;
        font: 1.15rem/1.6 var(--reading);
    }
    .story-meta {
        color: var(--ink-soft);
        font: 0.75rem/1.6 var(--sans);
    }
    .story-copy ol {
        display: grid;
        gap: 0;
        margin: 2rem 0;
        padding: 0;
        border-top: 1px solid var(--line);
        list-style: none;
    }
    .story-copy li {
        border-bottom: 1px solid var(--line);
    }
    .story-copy li button {
        width: 100%;
        text-align: left;
        border: 0;
        background: transparent;
        cursor: pointer;
        display: grid;
        grid-template-columns: 1.4rem minmax(0, 1fr);
        gap: 0.8rem;
        padding: 1.2rem 0.6rem;
        border-bottom: 1px solid var(--line);
    }
    .story-copy li button.active {
        background: var(--paper-deep);
    }
    .story-copy li button.active p {
        color: var(--ink);
    }
    .story-copy li button.active span {
        color: var(--paper);
        background: var(--madder);
    }
    .story-copy li span {
        display: grid;
        place-items: center;
        width: 1.3rem;
        height: 1.3rem;
        color: var(--madder);
        border: 1px solid var(--madder);
        border-radius: 50%;
        font: 500 0.75rem var(--sans);
    }
    .story-copy li p {
        margin: 0;
        color: var(--ink-soft);
        font: 1.05rem/1.6 var(--reading);
    }
    .story-copy > a {
        display: inline-flex;
        gap: 0.4rem;
        align-items: center;
        min-height: 2.75rem;
        font: 500 0.75rem var(--sans);
        text-underline-offset: 0.3rem;
    }
    .gallery-heading {
        margin-top: clamp(4rem, 8vw, 9rem);
        padding-top: 2rem;
        border-top: 1px solid var(--ink);
    }
    .gallery-heading h3 {
        display: flex;
        gap: 1rem;
        align-items: baseline;
        margin: 0 0 3rem;
        font: 400 clamp(2.4rem, 5vw, 5.5rem)/1 var(--editorial-font);
        letter-spacing: var(--display-tracking, -0.03em);
    }
    .gallery-heading h3 span {
        color: var(--madder);
        font: 600 0.72em/1 var(--sans);
        font-variant-numeric: tabular-nums;
        letter-spacing: -0.035em;
    }
    .media-gallery {
        display: grid;
        grid-template-columns: repeat(3, minmax(0, 1fr));
        gap: 3.5rem clamp(1.5rem, 3.5vw, 4rem);
        align-items: start;
        row-gap: 0;
    }
    .media-gallery article {
        min-width: 0;
        padding-bottom: 3.5rem;
    }

    .media-gallery button {
        position: relative;
        display: block;
        place-items: center;
        width: 100%;
        height: auto;
        padding: 0;
        overflow: hidden;
        border: 0;
        background: #ebe7de;
        cursor: zoom-in;
    }
    .media-gallery img {
        min-width: 0;
        min-height: 0;
        width: 100%;
        height: auto;
        object-fit: contain;
        transition: transform 300ms ease;
    }
    .media-gallery button:hover img {
        transform: scale(1.025);
    }
    .media-gallery button span {
        position: absolute;
        right: 0.8rem;
        bottom: 0.8rem;
        display: flex;
        gap: 0.35rem;
        align-items: center;
        padding: 0.55rem 0.7rem;
        color: #f7f5ef;
        background: #171711;
        font: 500 0.75rem var(--sans);
        opacity: 0;
        transition: opacity 160ms ease;
    }
    .media-gallery button:hover span,
    .media-gallery button:focus-visible span {
        opacity: 1;
    }
    .media-gallery article > div {
        padding-top: 1rem;
    }
    .media-gallery h4 {
        margin: 0 0 0.55rem;
        font: italic 400 clamp(1.4rem, 1.8vw, 1.9rem)/1.16 var(--editorial-font);
        letter-spacing: -0.02em;
    }
    .media-gallery article > div span {
        color: var(--ink-soft);
        font: 0.75rem var(--sans);
    }
    dialog {
        position: fixed;
        inset: 0;
        display: grid;
        place-items: center;
        width: 100%;
        height: 100%;
        max-width: none;
        max-height: none;
        margin: 0;
        padding: 1.5rem;
        overflow-y: auto;
        border: 0;
        background: #10110fed;
    }
    dialog::backdrop {
        background: transparent;
    }
    .media-modal {
        position: relative;
        display: grid;
        grid-template-columns: minmax(0, 1.4fr) minmax(21rem, 0.75fr);
        width: min(94rem, 100%);
        height: min(54rem, calc(100svh - 3rem));
        overflow: hidden;
        color: #f7f5ef;
        background: #1c1e1a;
    }
    .media-modal > button {
        position: absolute;
        z-index: 2;
        top: 0.8rem;
        right: 0.8rem;
        display: grid;
        place-items: center;
        width: 2.75rem;
        height: 2.75rem;
        padding: 0;
        color: #f7f5ef;
        border: 1px solid #f7f5ef60;
        background: #171711;
        cursor: pointer;
    }
    .media-modal figure {
        position: relative;
        display: grid;
        place-items: center;
        min-height: 0;
        margin: 0;
        padding: 1.5rem 1.5rem 5rem;
        overflow: hidden;
        background: #0e100d;
    }
    .media-modal figure > img {
        min-width: 0;
        min-height: 0;
        width: 100%;
        height: 100%;
        max-height: calc(100svh - 9.5rem);
        object-fit: contain;
    }
    .image-navigation {
        position: absolute;
        right: 1.5rem;
        bottom: 1rem;
        left: 1.5rem;
        display: flex;
        align-items: center;
        justify-content: space-between;
    }
    .image-navigation button {
        display: grid;
        place-items: center;
        width: 2.75rem;
        height: 2.75rem;
        color: #f7f5ef;
        border: 1px solid #f7f5ef60;
        background: none;
        cursor: pointer;
    }
    .image-navigation button:disabled {
        opacity: 0.25;
        cursor: default;
    }
    .image-navigation span {
        color: #bdbfb8;
        font: 0.75rem var(--sans);
        font-variant-numeric: tabular-nums;
    }
    .media-modal > div {
        padding: 4rem clamp(1.5rem, 3vw, 3rem) 2rem;
        overflow-y: auto;
    }
    .media-modal .eyebrow {
        color: #d88976;
    }
    .media-modal h2 {
        margin: 0 0 1.25rem;
        font: 400 clamp(2.2rem, 3.3vw, 4.1rem)/1.03 var(--editorial-font);
        letter-spacing: var(--display-tracking, -0.03em);
    }
    .modal-caption {
        color: #d6d8d1;
        font: 1.15rem/1.6 var(--reading);
    }
    .media-modal dl {
        margin: 2rem 0;
        border-top: 1px solid #f7f5ef26;
    }
    .media-modal dl div {
        display: grid;
        grid-template-columns: 5rem minmax(0, 1fr);
        gap: 1rem;
        padding: 0.85rem 0;
        border-bottom: 1px solid #f7f5ef26;
    }
    .media-modal dt {
        color: #a7aaa2;
        font: 0.75rem/1.5 var(--sans);
    }
    .media-modal dd {
        margin: 0;
        color: #f7f5ef;
        font: 1.05rem/1.55 var(--reading);
        overflow-wrap: anywhere;
    }
    .media-modal .button {
        min-height: 2.8rem;
        padding: 0.7rem 1rem;
        color: #f7f5ef;
        border: 1px solid #f7f5ef60;
        background: none;
        font-size: 0.75rem;
    }
    @media (max-width: 1000px) {
        .story,
        .story:nth-child(even) {
            grid-template-columns: minmax(0, 1fr) minmax(16rem, 0.65fr);
            gap: 2rem;
        }
        .story:nth-child(even) figure {
            grid-column: 1;
        }
        .story:nth-child(even) .story-copy {
            grid-column: 2;
            grid-row: 1;
        }
        .media-gallery {
            grid-template-columns: repeat(2, minmax(0, 1fr));
            row-gap: 0;
        }
    }
    @media (max-width: 800px) {
        .story,
        .story:nth-child(even) {
            display: block;
        }
        .story-copy {
            position: static;
            margin-top: 2rem;
        }
        .story-copy h3 {
            max-width: 26ch;
            font-size: 2.5rem;
        }
        .caption,
        .story-meta,
        .story-copy ol {
            max-width: 40rem;
        }
        .media-modal {
            display: block;
            height: auto;
            max-height: calc(100svh - 3rem);
            overflow-y: auto;
        }
        .media-modal figure > img {
            height: auto;
            max-height: 65svh;
        }
        .media-modal > div {
            padding-top: 2rem;
            overflow: visible;
        }
        .media-modal > button {
            position: fixed;
            top: 2.3rem;
            right: 2.3rem;
        }
    }
    @media (max-width: 600px) {
        .evidence-heading h2 {
            font-size: clamp(3.4rem, 15vw, 5rem);
        }
        .stories {
            gap: 4rem;
        }
        .story-copy h3 {
            font-size: 2rem;
        }
        .annotation-box {
            border-width: 1px;
        }
        .annotation-box i {
            top: -0.5rem;
            left: -0.5rem;
            width: 1.4rem;
            height: 1.4rem;
            font-size: 0.75rem;
        }
        .media-gallery {
            gap: 2.5rem 1.25rem;
            row-gap: 0;
        }

        .media-gallery h4 {
            font-size: 1.4rem;
        }
        .media-gallery article > div span {
            font-size: 0.75rem;
        }
        .media-gallery button span {
            right: 0.4rem;
            bottom: 0.4rem;
            padding: 0.4rem;
            font-size: 0.75rem;
        }
        .media-gallery button span :global(svg) {
            display: none;
        }
        dialog {
            padding: 0.75rem;
        }
        .media-modal {
            max-height: calc(100svh - 1.5rem);
        }
        .media-modal > button {
            top: 1.5rem;
            right: 1.5rem;
        }
        .media-modal figure {
            padding: 0.75rem 0.75rem 4.5rem;
        }
        .media-modal > div {
            padding: 2rem 1.25rem;
        }
    }
    @media (max-width: 360px) {
        .media-gallery {
            grid-template-columns: 1fr;
            row-gap: 0;
        }
    }
    @media (hover: none) {
        .media-gallery button span {
            opacity: 1;
        }
    }
    @media (prefers-reduced-motion: reduce) {
        .media-gallery img,
        .media-gallery button span {
            transition: none;
        }
    }
</style>
