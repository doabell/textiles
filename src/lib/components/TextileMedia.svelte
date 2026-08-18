<script lang="ts">
    import { ExternalLink, Info, X } from "@lucide/svelte";
    import type { TextileGalleryItem, TextileMedia } from "$lib/data/original-textile-media";
    import { assetPath } from "$lib/utils/asset-path";

    let { media, textileName }: { media: TextileMedia; textileName: string } = $props();

    let selected = $state<TextileGalleryItem | null>(null);

    function handleKeydown(event: KeyboardEvent) {
        if (event.key === "Escape") selected = null;
    }
</script>

<svelte:window onkeydown={handleKeydown} />

<section class="visual-evidence">
    <div class="page-shell evidence-grid">
        <div class="section-index">
            <span>03</span>
            <p>Visual evidence</p>
        </div>

        <header class="evidence-heading">
            <h2>{textileName} in material and visual culture</h2>
        </header>

        <div class="stories">
            {#each media.stories as story, storyIndex}
                <article class="story">
                    <figure style={`--ratio: ${story.width} / ${story.height}`}>
                        <img
                            src={assetPath(story.image)}
                            alt={story.title}
                            loading="lazy"
                            decoding="async"
                        />
                        {#each story.annotations as annotation}
                            <span
                                class="annotation-box"
                                style={`--left: ${annotation.left}%; --top: ${annotation.top}%; --width: ${annotation.width}%; --height: ${annotation.height}%`}
                                aria-hidden="true"
                            >
                                <i>{annotation.number}</i>
                            </span>
                        {/each}
                    </figure>

                    <div class="story-copy">
                        <p class="story-number">View {String(storyIndex + 1).padStart(2, "0")}</p>
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
                                    <span>{annotation.number}</span>
                                    <p>{annotation.text}</p>
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
            <h3>{media.gallery.length} related works and textile samples</h3>
        </div>

        <div class="media-gallery">
            {#each media.gallery as item, index}
                <article>
                    <button type="button" onclick={() => (selected = item)}>
                        <img
                            src={assetPath(item.image)}
                            alt={item.title}
                            loading="lazy"
                            decoding="async"
                        />
                        <span><Info size={14} /> View details</span>
                    </button>
                    <div>
                        <p>{String(index + 1).padStart(2, "0")}</p>
                        <h4>{item.title}</h4>
                        {#if item.date}<span>{item.date}</span>{/if}
                    </div>
                </article>
            {/each}
        </div>
    </div>
</section>

{#if selected}
    <div
        class="media-modal-backdrop"
        role="presentation"
        onclick={(event) => {
            if (event.currentTarget === event.target) selected = null;
        }}
    >
        <div class="media-modal" role="dialog" aria-modal="true" aria-labelledby="media-title">
            <button
                type="button"
                aria-label="Close image details"
                onclick={() => (selected = null)}
            >
                <X size={20} />
            </button>
            <figure><img src={assetPath(selected.image)} alt={selected.title} /></figure>
            <div>
                <p class="eyebrow">{selected.type || "Visual record"}</p>
                <h2 id="media-title">{selected.title}</h2>
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
                {#if selected.catalogueUrl}
                    <a class="button" href={selected.catalogueUrl} target="_blank" rel="noreferrer">
                        Collection record <ExternalLink size={14} />
                    </a>
                {/if}
            </div>
        </div>
    </div>
{/if}

<style>
    .visual-evidence {
        padding: clamp(4rem, 8vw, 8rem) 0;
        color: var(--paper);
        background: #20241f;
    }

    .evidence-grid {
        display: grid;
        grid-template-columns: minmax(8rem, 0.28fr) minmax(0, 1fr);
        gap: 3rem clamp(2rem, 7vw, 8rem);
    }

    .section-index {
        display: flex;
        gap: 1rem;
        align-items: baseline;
        align-self: start;
        padding-top: 0.5rem;
        border-top: 1px solid rgba(244, 239, 229, 0.28);
    }

    .section-index span {
        color: var(--saffron);
        font-size: 0.64rem;
    }

    .section-index p {
        margin: 0;
        color: rgba(244, 239, 229, 0.62);
        font-size: 0.58rem;
        letter-spacing: 0.08em;
        text-transform: uppercase;
    }

    .evidence-heading {
        max-width: 56rem;
    }

    .media-modal .eyebrow {
        color: var(--saffron);
    }

    .evidence-heading h2 {
        max-width: 14ch;
        margin-bottom: 1.5rem;
        font-family: var(--serif);
        font-size: clamp(3rem, 6vw, 6.4rem);
        font-weight: 400;
        letter-spacing: -0.055em;
        line-height: 0.92;
    }

    .stories,
    .gallery-heading,
    .media-gallery {
        grid-column: 1 / -1;
    }

    .stories {
        display: grid;
        gap: clamp(3rem, 7vw, 7rem);
        margin-top: clamp(1rem, 3vw, 3rem);
    }

    .story {
        display: grid;
        grid-template-columns: minmax(0, 1.35fr) minmax(20rem, 0.65fr);
        gap: clamp(1.5rem, 4vw, 4rem);
        align-items: start;
    }

    .story:nth-child(even) {
        grid-template-columns: minmax(20rem, 0.65fr) minmax(0, 1.35fr);
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
        background: #11130f;
    }

    .story figure > img {
        width: 100%;
        height: 100%;
        object-fit: contain;
    }

    .annotation-box {
        position: absolute;
        top: var(--top);
        left: var(--left);
        width: var(--width);
        height: var(--height);
        border: clamp(2px, 0.25vw, 4px) solid var(--paper);
        border-radius: 0.8rem;
        box-shadow:
            0 0 0 1px rgba(23, 23, 17, 0.5),
            0 0.4rem 1rem rgba(0, 0, 0, 0.16);
    }

    .annotation-box i {
        position: absolute;
        top: -0.75rem;
        left: -0.75rem;
        display: grid;
        place-items: center;
        width: 1.55rem;
        height: 1.55rem;
        color: var(--ink);
        border-radius: 50%;
        background: var(--paper);
        font-size: 0.65rem;
        font-style: normal;
        font-weight: 800;
    }

    .story-copy {
        padding-top: 0.4rem;
        border-top: 1px solid rgba(244, 239, 229, 0.25);
    }

    .story-number {
        color: var(--saffron);
        font-size: 0.58rem;
        letter-spacing: 0.08em;
        text-transform: uppercase;
    }

    .story-copy h3 {
        margin-bottom: 1rem;
        font-family: var(--serif);
        font-size: clamp(2rem, 3.6vw, 3.6rem);
        font-weight: 400;
        letter-spacing: -0.04em;
        line-height: 0.98;
    }

    .caption {
        color: rgba(244, 239, 229, 0.75);
        font-family: var(--reading);
        line-height: 1.58;
    }

    .story-meta {
        color: rgba(244, 239, 229, 0.52);
        font-size: 0.69rem;
        line-height: 1.5;
    }

    .story-copy ol {
        display: grid;
        gap: 0;
        margin: 1.5rem 0;
        padding: 0;
        border-top: 1px solid rgba(244, 239, 229, 0.2);
        list-style: none;
    }

    .story-copy li {
        display: grid;
        grid-template-columns: 1.8rem 1fr;
        gap: 0.7rem;
        padding: 1rem 0;
        border-bottom: 1px solid rgba(244, 239, 229, 0.16);
    }

    .story-copy li span {
        display: grid;
        place-items: center;
        width: 1.5rem;
        height: 1.5rem;
        color: var(--ink);
        border-radius: 50%;
        background: var(--paper);
        font-size: 0.62rem;
        font-weight: 800;
    }

    .story-copy li p {
        margin: 0;
        color: rgba(244, 239, 229, 0.7);
        font-family: var(--reading);
        font-size: 0.86rem;
        line-height: 1.55;
    }

    .story-copy > a {
        display: inline-flex;
        gap: 0.35rem;
        align-items: center;
        color: var(--paper);
        font-size: 0.68rem;
        text-underline-offset: 0.3rem;
    }

    .gallery-heading {
        margin-top: clamp(2rem, 5vw, 5rem);
        padding-top: clamp(3rem, 6vw, 6rem);
        border-top: 1px solid rgba(244, 239, 229, 0.25);
    }

    .gallery-heading h3 {
        max-width: 16ch;
        margin: 0;
        font-family: var(--serif);
        font-size: clamp(2.6rem, 5vw, 5rem);
        font-weight: 400;
        letter-spacing: -0.05em;
        line-height: 0.95;
    }

    .media-gallery {
        display: grid;
        grid-template-columns: repeat(3, minmax(0, 1fr));
        gap: 1px;
        border: 1px solid rgba(244, 239, 229, 0.22);
        background: rgba(244, 239, 229, 0.22);
    }

    .media-gallery article {
        min-width: 0;
        background: #292d27;
    }

    .media-gallery button {
        position: relative;
        width: 100%;
        aspect-ratio: 1.18;
        padding: 0;
        overflow: hidden;
        border: 0;
        background: #151713;
        cursor: zoom-in;
    }

    .media-gallery img {
        width: 100%;
        height: 100%;
        object-fit: cover;
        transition: transform 400ms ease;
    }

    .media-gallery button:hover img {
        transform: scale(1.025);
    }

    .media-gallery button span {
        position: absolute;
        right: 0.6rem;
        bottom: 0.6rem;
        display: flex;
        gap: 0.35rem;
        align-items: center;
        padding: 0.4rem 0.55rem;
        color: var(--paper);
        background: rgba(23, 23, 17, 0.84);
        font-size: 0.64rem;
        font-weight: 700;
        opacity: 0;
        transition: opacity 160ms ease;
    }

    .media-gallery button:hover span,
    .media-gallery button:focus-visible span {
        opacity: 1;
    }

    .media-gallery article > div {
        display: grid;
        grid-template-columns: 1.5rem 1fr;
        gap: 0.7rem;
        min-height: 8.5rem;
        padding: 1rem;
    }

    .media-gallery article > div p {
        margin: 0;
        color: var(--saffron);
        font-size: 0.56rem;
    }

    .media-gallery h4 {
        margin: 0 0 0.5rem;
        font-family: var(--serif);
        font-size: 1.15rem;
        font-weight: 400;
        line-height: 1.1;
    }

    .media-gallery article > div span {
        grid-column: 2;
        color: rgba(244, 239, 229, 0.54);
        font-size: 0.67rem;
    }

    .media-modal-backdrop {
        position: fixed;
        z-index: 120;
        inset: 0;
        display: grid;
        place-items: center;
        padding: 1rem;
        background: rgba(10, 10, 8, 0.86);
        backdrop-filter: blur(8px);
    }

    .media-modal {
        position: relative;
        display: grid;
        grid-template-columns: minmax(0, 1.2fr) minmax(20rem, 0.8fr);
        width: min(90rem, 100%);
        max-height: calc(100svh - 2rem);
        overflow: auto;
        color: var(--paper);
        background: #20241f;
        box-shadow: 0 2rem 6rem rgba(0, 0, 0, 0.4);
    }

    .media-modal > button {
        position: absolute;
        z-index: 2;
        top: 0.8rem;
        right: 0.8rem;
        display: grid;
        place-items: center;
        width: 2.7rem;
        height: 2.7rem;
        padding: 0;
        color: var(--ink);
        border: 0;
        background: var(--paper);
        cursor: pointer;
    }

    .media-modal figure {
        display: grid;
        place-items: center;
        min-height: 35rem;
        margin: 0;
        background: #11130f;
    }

    .media-modal figure img {
        width: 100%;
        height: 100%;
        max-height: calc(100svh - 2rem);
        object-fit: contain;
    }

    .media-modal > div {
        padding: clamp(2rem, 4vw, 4rem);
        overflow-y: auto;
    }

    .media-modal h2 {
        margin-bottom: 1.2rem;
        font-family: var(--serif);
        font-size: clamp(2.5rem, 4.5vw, 4.8rem);
        font-weight: 400;
        letter-spacing: -0.05em;
        line-height: 0.94;
    }

    .modal-caption {
        color: rgba(244, 239, 229, 0.72);
        font-family: var(--reading);
        line-height: 1.62;
    }

    .media-modal dl {
        margin: 2rem 0;
        border-top: 1px solid rgba(244, 239, 229, 0.2);
    }

    .media-modal dl div {
        display: grid;
        grid-template-columns: 6rem 1fr;
        gap: 1rem;
        padding: 0.8rem 0;
        border-bottom: 1px solid rgba(244, 239, 229, 0.14);
    }

    .media-modal dt {
        color: rgba(244, 239, 229, 0.48);
        font-size: 0.62rem;
        text-transform: uppercase;
    }

    .media-modal dd {
        margin: 0;
        color: rgba(244, 239, 229, 0.76);
        font-family: var(--reading);
        font-size: 0.83rem;
    }

    .media-modal .button {
        color: var(--ink);
        border-color: var(--paper);
        background: var(--paper);
    }

    @media (max-width: 900px) {
        .evidence-grid,
        .story,
        .story:nth-child(even) {
            grid-template-columns: 1fr;
        }

        .story:nth-child(even) figure,
        .story:nth-child(even) .story-copy {
            grid-column: 1;
            grid-row: auto;
        }

        .media-gallery {
            grid-template-columns: repeat(2, minmax(0, 1fr));
        }

        .media-modal {
            grid-template-columns: 1fr;
        }

        .media-modal figure {
            min-height: 45svh;
        }
    }

    @media (max-width: 580px) {
        .media-gallery {
            grid-template-columns: 1fr;
        }

        .story figure {
            border-radius: 0;
        }

        .annotation-box i {
            top: -0.55rem;
            left: -0.55rem;
            width: 1.2rem;
            height: 1.2rem;
            font-size: 0.55rem;
        }
    }
</style>
