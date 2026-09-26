<script lang="ts">
    import { ArrowDown, ArrowUpRight } from "@lucide/svelte";
    import { featuredTextiles } from "$lib/data/textiles";
    import { projectTools } from "$lib/data/projects";
    import { assetPath } from "$lib/utils/asset-path";
</script>

<svelte:head>
    <title>The Dutch Textile Trade Project</title>
    <meta
        name="description"
        content="This project aims to understand the circulation of globally-sourced textiles on Dutch ships around the world in the seventeenth and eighteenth centuries by examining data drawn from trade records alongside samples of textiles and visual culture depicting textiles in use."
    />
</svelte:head>

<section class="opening">
    <div class="masthead page-shell">
        <h1>
            <span class="title-dutch">The Dutch</span><span class="title-trade">Textile Trade</span
            ><span class="title-project">Project</span>
        </h1>
        <a class="opening-jump" href="#introduction" aria-label="About the project"
            ><ArrowDown size={28} strokeWidth={1.25} /></a
        >
    </div>
    <a class="cover" href="/textiles/patola/" aria-label="View Patola">
        <img
            src="/textile-media/patola/story-02.jpg"
            alt=""
            width="1649"
            height="806"
            fetchpriority="high"
        />
        <span class="cover-link"
            >Patola <ArrowUpRight size={25} strokeWidth={1.3} aria-hidden="true" /></span
        >
    </a>
    <div class="introduction page-shell" id="introduction">
        <h2>Welcome to the Dutch Textile Trade Project.</h2>
        <div>
            <p>
                This project aims to understand the circulation of globally-sourced textiles on
                Dutch ships around the world in the seventeenth and eighteenth centuries by
                examining data drawn from trade records alongside samples of textiles and visual
                culture depicting textiles in use.
            </p>
            <a class="round-link" href="/textiles/"
                ><span>Visual Textile Glossary</span><span class="round-arrow"
                    ><ArrowUpRight size={24} strokeWidth={1.3} aria-hidden="true" /></span
                ></a
            >
        </div>
        <p class="award">
            Winner of the Renaissance Society of America’s 2024 <a
                href="https://www.rsa.org/page/DigitalInnovationAward"
                target="_blank"
                rel="noreferrer"
                >Digital Innovation Award <ArrowUpRight size={12} aria-hidden="true" /></a
            >
        </p>
    </div>
</section>

<section class="glossary-section page-shell">
    <div class="section-heading">
        <h2>Visual Textile<br /><em>Glossary</em></h2>
        <div>
            <p>
                The centerpiece of this project, the Visual Textile Glossary, provides each
                historical textile term with a short definition and a longer essay contextualizing
                that textile’s production and circulation. Each essay also includes visual and
                material examples, an interactive web application, and open access data.
            </p>
            <a class="text-link" href="/textiles/"
                >All textiles <ArrowUpRight size={18} aria-hidden="true" /></a
            >
        </div>
    </div>
    <div class="featured-grid">
        {#each featuredTextiles.slice(0, 4) as textile}
            <a class="textile-card" href={"/textiles/" + textile.slug + "/"}>
                <figure><img src={assetPath(textile.image)} alt="" loading="lazy" /></figure>
                <div>
                    <h3>{textile.shortName}</h3>
                    <ArrowUpRight size={23} strokeWidth={1.4} aria-hidden="true" />
                </div>
            </a>
        {/each}
    </div>
</section>

<section class="tools-section">
    <div class="page-shell">
        <div class="section-heading">
            <h2>Data<br /><em>Visualization</em></h2>
            <p>
                These dynamic apps allow users to explore the project data by geography, date,
                company, textile name and/or descriptors of the textiles found in archival sources
                (modifiers), and value of textiles, producing data visualizations.
            </p>
        </div>
        <div class="tool-list">
            {#each projectTools as tool}
                <a href={tool.href} class="tool-item">
                    <h3>{tool.title}</h3>
                    {#if tool.description}<p>{tool.description}</p>{/if}
                    <span class="tool-arrow"
                        ><ArrowUpRight size={30} strokeWidth={1.3} aria-hidden="true" /></span
                    >
                </a>
            {/each}
        </div>
    </div>
</section>

<style>
    .opening {
        padding-top: clamp(2rem, 4vw, 4rem);
    }
    .masthead {
        position: relative;
        padding-bottom: clamp(1.5rem, 3vw, 3rem);
    }
    h1 {
        display: grid;
        grid-template-columns: 1fr auto;
        align-items: baseline;
        margin: 0;
        font-weight: var(--display-weight, 500);
    }
    h1 span {
        display: block;
    }
    .title-dutch {
        font-family: var(--editorial-font);
        font-size: clamp(3.6rem, 8.6vw, 8.8rem);
        font-style: italic;
        font-weight: 350;
        letter-spacing: -0.045em;
        line-height: 1;
    }
    .title-trade {
        grid-column: 1 / -1;
        grid-row: 2;
        font-family: var(--sans);
        font-size: clamp(3.3rem, 11.7vw, 11.7rem);
        letter-spacing: -0.055em;
        line-height: 1.05;
        text-transform: uppercase;
    }
    .title-project {
        grid-column: 2;
        grid-row: 1;
        padding-right: 7rem;
        font-family: var(--editorial-font);
        font-size: clamp(2rem, 4.2vw, 4.5rem);
        font-style: italic;
        font-weight: 350;
        letter-spacing: -0.035em;
    }
    .opening-jump {
        position: absolute;
        top: 1.6rem;
        right: var(--page-pad);
        display: grid;
        place-items: center;
        width: 4rem;
        height: 4rem;
        border: 1px solid var(--line-strong);
        border-radius: 50%;
        transition:
            color 200ms,
            background 200ms,
            transform 200ms;
    }
    .opening-jump:hover {
        color: var(--paper);
        background: var(--ink);
        transform: translateY(4px);
    }
    .cover {
        position: relative;
        display: block;
        margin: 0 clamp(0.75rem, 2vw, 2rem);
        aspect-ratio: 2.08;
        overflow: hidden;
        background: #311d1b;
    }
    .cover > img {
        width: 100%;
        height: 100%;
        object-fit: cover;
        transition: transform 900ms cubic-bezier(0.2, 0.7, 0.2, 1);
    }
    .cover:hover > img {
        transform: scale(1.025);
    }
    .cover-link {
        position: absolute;
        bottom: 1.5rem;
        right: 1.5rem;
        display: flex;
        align-items: center;
        justify-content: space-between;
        gap: 2.5rem;
        padding: 1.2rem 1.4rem;
        color: var(--ink);
        background: var(--paper);
        font-family: var(--sans);
        font-size: 0.85rem;
    }
    .introduction {
        display: grid;
        grid-template-columns: 1fr 1fr;
        gap: clamp(2rem, 6vw, 6rem);
        padding-top: clamp(3rem, 6vw, 6rem);
        padding-bottom: clamp(3rem, 6vw, 6rem);
    }
    .introduction h2 {
        max-width: 15ch;
        margin: 0;
        font-family: var(--editorial-font);
        font-size: clamp(2.25rem, 4vw, 4.4rem);
        font-weight: var(--display-weight, 400);
        letter-spacing: var(--display-tracking, -0.045em);
        line-height: 1.1;
    }
    .introduction > div > p {
        max-width: 48ch;
        margin: 0;
        font-family: var(--reading);
        font-size: clamp(1.25rem, 1.8vw, 1.6rem);
        line-height: 1.6;
    }
    .round-link {
        display: flex;
        align-items: center;
        justify-content: space-between;
        max-width: 28rem;
        gap: 1.5rem;
        margin-top: 2rem;
        padding-top: 1rem;
        border-top: 1px solid var(--line);
        font-family: var(--sans);
        font-size: 0.88rem;
        font-weight: 550;
        text-decoration: none;
    }
    .round-arrow {
        display: grid;
        place-items: center;
        width: 3.4rem;
        height: 3.4rem;
        border: 1px solid var(--line-strong);
        border-radius: 50%;
        transition:
            color 200ms,
            background 200ms;
    }
    .round-link:hover .round-arrow {
        color: var(--paper);
        background: var(--madder);
        border-color: var(--madder);
    }
    .award {
        grid-column: 1 / -1;
        display: flex;
        flex-wrap: wrap;
        align-items: baseline;
        gap: 0.35rem;
        margin: 0;
        color: var(--ink-soft);
        font-size: 0.85rem;
    }
    .award a {
        display: inline-flex;
        align-items: center;
        gap: 0.4rem;
        text-underline-offset: 4px;
    }
    .glossary-section {
        padding-top: clamp(3rem, 6vw, 6rem);
        padding-bottom: clamp(5rem, 10vw, 10rem);
        border-top: 1px solid var(--line);
    }
    .section-heading {
        display: grid;
        grid-template-columns: 1.1fr 0.9fr;
        align-items: end;
        gap: clamp(2rem, 6vw, 6rem);
        margin-bottom: 4rem;
    }
    .section-heading h2 {
        margin: 0;
        font-family: var(--display-font, var(--sans));
        font-size: clamp(3rem, 5.5vw, 5.6rem);
        font-weight: var(--display-weight, 500);
        letter-spacing: var(--display-tracking, -0.045em);
        line-height: 1.05;
    }
    .section-heading h2 em {
        font-family: var(--editorial-font);
        font-size: 1.2em;
        letter-spacing: var(--display-tracking, -0.035em);
        font-weight: var(--display-weight, 400);
    }
    .section-heading p {
        max-width: 52ch;
        margin: 0;
        color: var(--ink-soft);
        font-family: var(--reading);
        font-size: clamp(1.1875rem, 1.4vw, 1.375rem);
        line-height: 1.65;
    }
    .section-heading .text-link {
        margin-top: 1.5rem;
    }
    .featured-grid {
        display: grid;
        grid-template-columns: repeat(4, minmax(0, 1fr));
        align-items: start;
        gap: 1.5rem;
    }
    .textile-card {
        min-width: 0;
        text-decoration: none;
    }
    .textile-card:nth-child(even) {
        margin-top: 7rem;
    }
    .textile-card figure {
        aspect-ratio: 0.68;
        margin: 0;
        overflow: hidden;
        background: var(--paper-deep);
    }
    .textile-card img {
        width: 100%;
        height: 100%;
        object-fit: cover;
        transition: transform 700ms cubic-bezier(0.2, 0.7, 0.2, 1);
    }
    .textile-card:hover img {
        transform: scale(1.045);
    }
    .textile-card > div {
        display: flex;
        justify-content: space-between;
        align-items: start;
        gap: 0.7rem;
        padding: 1.25rem 0;
        border-bottom: 1px solid var(--line-strong);
    }
    .textile-card h3 {
        margin: 0;
        font-family: var(--editorial-font);
        font-size: clamp(1.4rem, 2.4vw, 2.4rem);
        line-height: 1.1;
        font-weight: var(--display-weight, 400);
        letter-spacing: var(--display-tracking, -0.03em);
    }
    .textile-card :global(svg) {
        flex-shrink: 0;
        margin-top: 0.2rem;
    }
    .tools-section {
        padding: clamp(4rem, 7vw, 7rem) 0;
        color: var(--inverse-ink);
        background: var(--inverse-surface);
    }
    .tools-section .section-heading p {
        color: var(--inverse-muted);
    }
    .tool-list {
        border-top: 1px solid #66665d;
    }
    .tool-item {
        display: grid;
        grid-template-columns: minmax(0, 1fr) minmax(0, 1fr) auto;
        align-items: center;
        gap: clamp(1.5rem, 4vw, 4rem);
        padding: 2.7rem 0;
        border-bottom: 1px solid #55554d;
        text-decoration: none;
    }
    .tool-item h3 {
        margin: 0;
        font-family: var(--display-font, var(--sans));
        font-size: clamp(1.6rem, 3vw, 2.8rem);
        font-weight: var(--display-weight, 450);
        letter-spacing: var(--display-tracking, -0.035em);
        line-height: 1.1;
        transition: color 200ms;
    }
    .tool-item p {
        margin: 0;
        color: var(--inverse-muted);
        font-family: var(--reading);
        font-size: 1.1875rem;
        line-height: 1.5;
    }
    .tool-arrow {
        grid-column: 3;
        display: grid;
        place-items: center;
        width: 4rem;
        height: 4rem;
        border: 1px solid #66665d;
        border-radius: 50%;
        transition:
            background 200ms,
            color 200ms,
            transform 200ms;
    }
    .tool-item:hover .tool-arrow {
        color: var(--ink);
        background: var(--paper);
        transform: rotate(45deg);
    }
    @media (prefers-reduced-motion: no-preference) {
        .title-dutch,
        .title-trade,
        .title-project {
            animation: enter 800ms cubic-bezier(0.2, 0.7, 0.2, 1) both;
        }
        .title-trade {
            animation-delay: 80ms;
        }
        .title-project {
            animation-delay: 160ms;
        }
        @keyframes enter {
            from {
                opacity: 0;
                transform: translateY(30px);
            }
            to {
                opacity: 1;
                transform: translateY(0);
            }
        }
    }
    @media (max-width: 900px) {
        .title-project {
            padding-right: 5rem;
        }
        .opening-jump {
            top: 0.4rem;
            width: 3rem;
            height: 3rem;
        }
        .featured-grid {
            gap: 1rem;
        }
        .textile-card:nth-child(even) {
            margin-top: 4rem;
        }
        .section-heading {
            gap: 2rem;
        }
    }
    @media (max-width: 680px) {
        .opening {
            padding-top: 2rem;
        }
        .title-dutch {
            font-size: 13vw;
        }
        .title-trade {
            font-size: 11.35vw;
            margin-top: 0.5rem;
        }
        .title-project {
            padding-right: 0;
            font-size: 7.4vw;
        }
        .opening-jump {
            display: none;
        }
        .cover {
            aspect-ratio: 1.12;
        }
        .cover-link {
            bottom: 0.75rem;
            right: 0.75rem;
            padding: 0.8rem 1rem;
            gap: 1.4rem;
        }
        .introduction,
        .section-heading {
            grid-template-columns: 1fr;
            gap: 1.7rem;
        }
        .introduction h2 {
            max-width: 16ch;
            font-size: 2.4rem;
        }
        .introduction > div > p {
            font-size: 1.25rem;
        }
        .award {
            margin-top: 1rem;
            line-height: 1.5;
        }
        .section-heading {
            margin-bottom: 2.5rem;
        }
        .section-heading h2 {
            font-size: 12vw;
        }
        .featured-grid {
            grid-template-columns: repeat(2, minmax(0, 1fr));
            gap: 2rem 1rem;
        }
        .textile-card:nth-child(even) {
            margin-top: 3.5rem;
        }
        .textile-card figure {
            aspect-ratio: 0.75;
        }
        .textile-card h3 {
            font-size: 1.5rem;
        }
        .tool-item {
            grid-template-columns: 1fr auto;
            gap: 1rem 1.5rem;
            padding: 2rem 0;
        }
        .tool-item p {
            grid-column: 1;
            grid-row: 2;
        }
        .tool-arrow {
            grid-column: 2;
            grid-row: 1;
            width: 3rem;
            height: 3rem;
        }
    }
</style>
