<script lang="ts">
    import {
        ArrowDown,
        ArrowRight,
        ArrowUpRight,
        BookOpenText,
        ChartNoAxesColumnIncreasing,
        Database,
        Search,
        Waypoints,
    } from "@lucide/svelte";
    import { featuredTextiles } from "$lib/data/textiles";
    import { assetPath } from "$lib/utils/asset-path";
    import heroSwatch from "../../pictures/img/4JWIvgu.jpg?url";
    import heroPortrait from "../../pictures/img/mTaaKjq.jpg?url";
    import archivePortrait from "../../pictures/img/FQelSi1.jpg?url";

    const evidence = [
        {
            number: "01",
            title: "Trade records",
            body: "Data drawn from trade records.",
            icon: BookOpenText,
        },
        {
            number: "02",
            title: "Samples of textiles",
            body: "Samples of textiles.",
            icon: Search,
        },
        {
            number: "03",
            title: "Visual culture",
            body: "Visual culture depicting textiles in use.",
            icon: Waypoints,
        },
        {
            number: "04",
            title: "Open access data",
            body: "Each essay also includes visual and material examples, an interactive web application, and open access data.",
            icon: Database,
        },
    ];
</script>

<svelte:head>
    <title>The Dutch Textile Trade Project</title>
    <meta
        name="description"
        content="This project aims to understand the circulation of globally-sourced textiles on Dutch ships around the world in the seventeenth and eighteenth centuries by examining data drawn from trade records alongside samples of textiles and visual culture depicting textiles in use."
    />
</svelte:head>

<section class="hero page-shell">
    <div class="hero-copy">
        <p class="award">
            Winner of the Renaissance Society of America’s 2024
            <a
                href="https://www.rsa.org/page/DigitalInnovationAward"
                target="_blank"
                rel="noreferrer">Digital Innovation Award</a
            >
        </p>
        <p class="eyebrow">Welcome to the Dutch Textile Trade Project.</p>
        <h1>The Dutch Textile Trade Project</h1>
        <p class="hero-lede">
            This project aims to understand the circulation of globally-sourced textiles on Dutch
            ships around the world in the seventeenth and eighteenth centuries by examining data
            drawn from trade records alongside samples of textiles and visual culture depicting
            textiles in use.
        </p>
        <div class="hero-actions">
            <a class="button" href="/textiles/">
                Enter the visual glossary <ArrowRight size={16} strokeWidth={1.8} />
            </a>
            <a class="button secondary" href="/map/">
                Open the textile map <Waypoints size={15} strokeWidth={1.8} />
            </a>
        </div>
        <a class="scroll-cue" href="#project">
            Visual Textile Glossary <ArrowDown size={14} />
        </a>
    </div>

    <div class="hero-visual" aria-label="Textile and pictorial records">
        <figure class="hero-swatch">
            <img src={assetPath(heroSwatch)} alt="" />
            <figcaption>
                <span>The Material Record</span>
                India · 1600–1800
            </figcaption>
        </figure>
        <figure class="hero-portrait">
            <img src={assetPath(heroPortrait)} alt="" />
            <figcaption>
                <span>Visual culture</span>
                Jean-Etienne Liotard · 1746
            </figcaption>
        </figure>
    </div>
</section>

<section class="stats-band" aria-label="Project at a glance">
    <div class="page-shell stats">
        <div>
            <strong>9,463</strong>
            <span>trade records</span>
        </div>
        <div>
            <strong>217</strong>
            <span>textile names in this dataset</span>
        </div>
        <div>
            <strong>25</strong>
            <span>years · 1700–1724</span>
        </div>
        <div>
            <strong>2</strong>
            <span>company archives · VOC & WIC</span>
        </div>
    </div>
</section>

<section class="project-intro page-shell" id="project">
    <div class="section-marker">
        <span>01</span>
        <p>About the project</p>
    </div>
    <div class="project-statement">
        <p class="eyebrow">The centerpiece of this project</p>
        <h2>Visual Textile Glossary</h2>
        <p class="project-lede">
            The centerpiece of this project, the Visual Textile Glossary, provides each historical
            textile term with a short definition and a longer essay contextualizing that textile’s
            production and circulation. Each essay also includes visual and material examples, an
            interactive web application, and open access data.
        </p>
    </div>

    <div class="evidence-grid">
        {#each evidence as item}
            <article>
                <div class="evidence-top">
                    <span>{item.number}</span>
                    <svelte:component
                        this={item.icon}
                        size={20}
                        strokeWidth={1.5}
                        aria-hidden="true"
                    />
                </div>
                <h3>{item.title}</h3>
                <p>{item.body}</p>
            </article>
        {/each}
    </div>
</section>

<section class="glossary-section">
    <div class="page-shell">
        <div class="section-heading">
            <div>
                <p class="eyebrow">Visual textile glossary</p>
                <h2>Visual Textile Glossary</h2>
            </div>
            <p>
                The centerpiece of this project, the Visual Textile Glossary, provides each
                historical textile term with a short definition and a longer essay contextualizing
                that textile’s production and circulation. Each essay also includes visual and
                material examples, an interactive web application, and open access data.
            </p>
        </div>

        <div class="featured-grid">
            {#each featuredTextiles.slice(0, 4) as textile, index}
                <a class="textile-card" href={`/textiles/${textile.slug}/`}>
                    <figure>
                        <img src={assetPath(textile.image)} alt="" />
                        <span>0{index + 1}</span>
                    </figure>
                    <div>
                        <h3>{textile.shortName}</h3>
                        <span class="card-action">Open entry <ArrowUpRight size={15} /></span>
                    </div>
                </a>
            {/each}
        </div>

        <a class="all-link" href="/textiles/">
            Browse all 14 glossary entries <ArrowRight size={16} />
        </a>
    </div>
</section>

<section class="tools-section page-shell">
    <div class="tools-heading">
        <p class="eyebrow">Data visualization</p>
        <h2>Data Visualization</h2>
        <p class="tools-summary">
            These dynamic apps allow users to explore the project data by geography, date, company,
            textile name and/or descriptors of the textiles found in archival sources (modifiers),
            and value of textiles, producing data visualizations.
        </p>
    </div>

    <div class="tool-list">
        <a href="/map/" class="tool-item indigo">
            <div class="tool-number">01</div>
            <div>
                <h3>Textile Geographies</h3>
                <p>
                    Search the textile data set by a range of archival modifiers—including color,
                    pattern, process, fiber, or quality and visualize this geographically and
                    infographically. Identify specific textile names of interest based on modifiers
                    and geography.
                </p>
            </div>
            <Waypoints size={30} strokeWidth={1.25} aria-hidden="true" />
            <span class="tool-arrow"><ArrowUpRight size={20} aria-hidden="true" /></span>
        </a>
        <a href="/values/" class="tool-item saffron">
            <div class="tool-number">02</div>
            <div>
                <h3>Textiles, Modifiers, and Values</h3>
                <p>
                    Explore specific textiles in greater detail, like the quantities, total values,
                    or per-piece values of imported or exported textiles over time or across
                    geographies. Users can compare different types (modifiers) of a given textile.
                </p>
            </div>
            <ChartNoAxesColumnIncreasing size={30} strokeWidth={1.25} aria-hidden="true" />
            <span class="tool-arrow"><ArrowUpRight size={20} aria-hidden="true" /></span>
        </a>
        <a href="/swatches/" class="tool-item madder">
            <div class="tool-number">03</div>
            <div>
                <h3>Swatch Search</h3>
                <p>
                    Explore our growing database of textile samples and swatches by textile name (if
                    you know it) or (if you don’t) search by attributes (modifiers) like color,
                    pattern, process, weave structure, and fiber.
                </p>
            </div>
            <Search size={30} strokeWidth={1.25} aria-hidden="true" />
            <span class="tool-arrow"><ArrowUpRight size={20} aria-hidden="true" /></span>
        </a>
    </div>
</section>

<section class="archive-context">
    <div class="context-image">
        <img src={assetPath(archivePortrait)} alt="" />
    </div>
    <div class="context-copy">
        <p class="eyebrow">Sources</p>
        <h2>Sources</h2>
        <p>
            Our data derives from Dutch East and West India Company archives and documents, which
            account for the movement and value of goods as they related to the Dutch economy, and
            which were produced from a Dutch perspective that justified territorial expansion and
            the enslavement of human beings in the name of trade, profit, and exploration.
        </p>
        <p>
            These documents are often dry and abstract, but it is imperative to us as scholars and
            as humans to provide reminders that these documents and this trade were and are not
            inert, neutral accounting.
        </p>
        <a class="text-link" href="/data/">
            Read how the data was made <ArrowRight size={15} />
        </a>
    </div>
</section>

<style>
    .hero {
        display: grid;
        grid-template-columns: minmax(0, 0.92fr) minmax(28rem, 1.08fr);
        gap: clamp(2.5rem, 6vw, 7rem);
        min-height: calc(100svh - 5.5rem);
        padding-top: clamp(3rem, 7vw, 7rem);
        padding-bottom: clamp(3rem, 6vw, 6rem);
    }

    .hero-copy {
        display: flex;
        flex-direction: column;
        align-items: flex-start;
        justify-content: center;
    }

    .award {
        margin-bottom: clamp(2.5rem, 6vw, 5.5rem);
        padding: 0.48rem 0.72rem;
        color: var(--indigo);
        border: 1px solid color-mix(in srgb, var(--indigo) 36%, transparent);
        font-family: var(--sans);
        font-size: 0.58rem;
        font-weight: 700;
        letter-spacing: 0.08em;
        text-transform: uppercase;
    }

    h1 {
        max-width: 10.5ch;
        margin-bottom: 1.7rem;
        font-family: var(--serif);
        font-size: clamp(4rem, 7.7vw, 8.3rem);
        font-weight: 400;
        letter-spacing: -0.065em;
        line-height: 0.86;
    }

    .hero-lede {
        max-width: 38rem;
        margin-bottom: 2rem;
        color: var(--ink-soft);
        font-family: var(--serif);
        font-size: clamp(1.1rem, 1.7vw, 1.35rem);
        line-height: 1.5;
    }

    .hero-actions {
        display: flex;
        flex-wrap: wrap;
        gap: 0.7rem;
    }

    .scroll-cue {
        display: inline-flex;
        align-items: center;
        gap: 0.5rem;
        margin-top: auto;
        padding-top: 3rem;
        color: var(--ink-soft);
        font-family: var(--sans);
        font-size: 0.61rem;
        letter-spacing: 0.06em;
        text-decoration: none;
        text-transform: uppercase;
    }

    .hero-visual {
        position: relative;
        min-height: 42rem;
    }

    .hero-visual figure {
        position: absolute;
        margin: 0;
        overflow: hidden;
        background: var(--paper-deep);
    }

    .hero-visual img {
        width: 100%;
        height: 100%;
        object-fit: cover;
        transition: transform 800ms cubic-bezier(0.2, 0.7, 0.2, 1);
    }

    .hero-visual:hover img {
        transform: scale(1.025);
    }

    .hero-swatch {
        top: 0;
        right: 0;
        left: 0;
        height: 60%;
    }

    .hero-swatch img {
        object-position: center;
    }

    .hero-portrait {
        right: 5%;
        bottom: 0;
        width: 43%;
        height: 52%;
        border: 0.7rem solid var(--paper);
    }

    .hero-portrait img {
        object-position: center 42%;
    }

    .hero-visual figcaption {
        position: absolute;
        right: 0;
        bottom: 0;
        left: 0;
        padding: 0.8rem 1rem;
        color: var(--cream);
        background: linear-gradient(transparent, rgba(15, 15, 12, 0.82));
        font-size: 0.62rem;
    }

    .hero-visual figcaption span {
        display: block;
        margin-bottom: 0.15rem;
        color: #e3bd70;
        font-family: var(--sans);
        font-size: 0.54rem;
        letter-spacing: 0.09em;
        text-transform: uppercase;
    }

    .stats-band {
        color: var(--paper);
        background: var(--indigo-deep);
    }

    .stats {
        display: grid;
        grid-template-columns: repeat(4, 1fr);
        padding-top: 1.6rem;
        padding-bottom: 1.6rem;
    }

    .stats > div {
        padding: 0 1.5rem;
        border-left: 1px solid rgba(244, 239, 229, 0.18);
    }

    .stats > div:first-child {
        padding-left: 0;
        border-left: 0;
    }

    .stats strong {
        display: block;
        font-family: var(--serif);
        font-size: clamp(1.7rem, 3vw, 2.65rem);
        font-weight: 400;
        line-height: 1;
    }

    .stats span {
        color: rgba(244, 239, 229, 0.64);
        font-family: var(--sans);
        font-size: 0.58rem;
        letter-spacing: 0.06em;
        text-transform: uppercase;
    }

    .project-intro {
        display: grid;
        grid-template-columns: 9rem 1fr;
        column-gap: clamp(2rem, 6vw, 7rem);
        padding-top: clamp(6rem, 11vw, 11rem);
        padding-bottom: clamp(6rem, 11vw, 11rem);
    }

    .section-marker {
        grid-row: span 2;
        color: var(--ink-soft);
        font-family: var(--sans);
        font-size: 0.61rem;
        letter-spacing: 0.08em;
        text-transform: uppercase;
    }

    .section-marker span {
        display: block;
        margin-bottom: 0.7rem;
        color: var(--madder);
    }

    .section-marker p {
        padding-top: 0.7rem;
        border-top: 1px solid var(--line-strong);
        writing-mode: vertical-rl;
    }

    .project-statement {
        display: grid;
        grid-template-columns: 1fr minmax(18rem, 0.5fr);
        gap: clamp(2rem, 6vw, 7rem);
    }

    .project-statement .eyebrow {
        grid-column: 1 / -1;
        margin-bottom: -1rem;
    }

    .project-statement h2,
    .tools-heading h2 {
        max-width: 10ch;
        margin-bottom: 0;
        font-family: var(--serif);
        font-size: clamp(3rem, 6vw, 6.5rem);
        font-weight: 400;
        letter-spacing: -0.055em;
        line-height: 0.94;
    }

    .project-lede {
        align-self: end;
        margin-bottom: 0;
        color: var(--ink-soft);
        font-family: var(--reading);
        font-size: clamp(1rem, 1.3vw, 1.12rem);
        line-height: 1.62;
    }

    .evidence-grid {
        display: grid;
        grid-template-columns: repeat(4, 1fr);
        grid-column: 2;
        margin-top: clamp(3rem, 7vw, 7rem);
        border-top: 1px solid var(--line-strong);
        border-bottom: 1px solid var(--line-strong);
    }

    .evidence-grid article {
        min-height: 17rem;
        padding: 1.25rem;
        border-left: 1px solid var(--line);
    }

    .evidence-grid article:first-child {
        border-left: 0;
    }

    .evidence-top {
        display: flex;
        justify-content: space-between;
        color: var(--madder);
        font-family: var(--sans);
        font-size: 0.62rem;
    }

    .evidence-grid h3 {
        margin-top: 5rem;
        margin-bottom: 0.55rem;
        font-family: var(--serif);
        font-size: 1.55rem;
        font-weight: 400;
    }

    .evidence-grid p {
        margin: 0;
        color: var(--ink-soft);
        font-size: 0.78rem;
        line-height: 1.55;
    }

    .glossary-section {
        padding-top: clamp(5rem, 10vw, 10rem);
        padding-bottom: clamp(5rem, 10vw, 9rem);
        background: var(--paper-deep);
    }

    .section-heading {
        display: grid;
        grid-template-columns: 1fr minmax(18rem, 0.45fr);
        gap: 3rem;
        align-items: end;
        margin-bottom: clamp(3rem, 6vw, 6rem);
    }

    .section-heading h2 {
        margin-bottom: 0;
        font-family: var(--serif);
        font-size: clamp(3.3rem, 7vw, 7rem);
        font-weight: 400;
        letter-spacing: -0.055em;
        line-height: 0.92;
    }

    .section-heading > p {
        margin: 0;
        color: var(--ink-soft);
        font-family: var(--reading);
        font-size: 1rem;
        line-height: 1.62;
    }

    .featured-grid {
        display: grid;
        grid-template-columns: repeat(4, 1fr);
        gap: 1px;
        background: var(--line-strong);
        border: 1px solid var(--line-strong);
    }

    .textile-card {
        min-width: 0;
        background: var(--paper-deep);
        text-decoration: none;
    }

    .textile-card figure {
        position: relative;
        aspect-ratio: 0.83;
        margin: 0;
        overflow: hidden;
        background: #d1c7b6;
    }

    .textile-card img {
        width: 100%;
        height: 100%;
        object-fit: cover;
        filter: saturate(0.82);
        transition:
            transform 500ms ease,
            filter 500ms ease;
    }

    .textile-card:hover img {
        filter: saturate(1);
        transform: scale(1.035);
    }

    .textile-card figure > span {
        position: absolute;
        top: 0;
        left: 0;
        display: grid;
        place-items: center;
        width: 2.25rem;
        height: 2.25rem;
        color: var(--paper);
        background: var(--ink);
        font-family: var(--sans);
        font-size: 0.57rem;
    }

    .textile-card > div {
        padding: 1.1rem;
        border-top: 1px solid var(--line-strong);
    }

    .textile-card h3 {
        margin-bottom: 1.4rem;
        font-family: var(--serif);
        font-size: clamp(1.35rem, 2.2vw, 2rem);
        font-weight: 400;
        line-height: 1;
    }

    .card-action {
        display: flex;
        align-items: center;
        justify-content: space-between;
        color: var(--ink-soft);
        font-size: 0.7rem;
        font-weight: 700;
    }

    .all-link {
        display: inline-flex;
        align-items: center;
        gap: 0.55rem;
        margin-top: 2rem;
        font-size: 0.82rem;
        font-weight: 700;
        text-underline-offset: 0.3rem;
    }

    .tools-heading {
        display: grid;
        grid-template-columns: minmax(0, 0.7fr) minmax(20rem, 0.55fr);
        gap: clamp(2rem, 7vw, 8rem);
        align-items: end;
        margin-bottom: clamp(2rem, 4vw, 3.5rem);
    }

    .tools-section {
        padding-top: clamp(5rem, 9vw, 8rem);
        padding-bottom: clamp(5rem, 9vw, 8rem);
    }

    .tools-heading .eyebrow {
        grid-column: 1 / -1;
        margin-bottom: -0.6rem;
    }

    .tools-heading h2 {
        max-width: none;
        font-size: clamp(2.8rem, 5.5vw, 5.8rem);
    }

    .tools-summary {
        margin: 0;
        color: var(--ink-soft);
        font-family: var(--reading);
        font-size: clamp(0.96rem, 1.2vw, 1.08rem);
        line-height: 1.62;
    }

    .tool-list {
        border-top: 1px solid var(--line-strong);
    }

    .tool-item {
        position: relative;
        display: grid;
        grid-template-columns: 3rem 1fr auto 2rem;
        gap: 2rem;
        align-items: center;
        min-height: 7.25rem;
        padding: 1.25rem 1.5rem;
        border-bottom: 1px solid var(--line-strong);
        text-decoration: none;
        transition:
            color 220ms ease,
            padding 220ms ease;
    }

    .tool-item::before {
        position: absolute;
        z-index: -1;
        inset: 0;
        content: "";
        transform: scaleY(0);
        transform-origin: bottom;
        transition: transform 260ms ease;
    }

    .tool-item.indigo::before {
        background: var(--indigo);
    }

    .tool-item.madder::before {
        background: var(--madder-dark);
    }

    .tool-item.saffron::before {
        background: var(--saffron);
    }

    .tool-item:hover {
        z-index: 1;
        padding-right: 2rem;
        padding-left: 2rem;
        color: var(--cream);
    }

    .tool-item:hover::before {
        transform: scaleY(1);
        transform-origin: top;
    }

    .tool-number {
        margin-bottom: 0.35rem;
        color: var(--ink-soft);
        font-family: var(--sans);
        font-size: 0.6rem;
        letter-spacing: 0.08em;
        text-transform: uppercase;
    }

    .tool-item:hover .tool-number,
    .tool-item:hover p {
        color: rgba(255, 255, 255, 0.68);
    }

    .tool-item h3 {
        margin-bottom: 0.25rem;
        font-family: var(--sans);
        font-size: clamp(1.15rem, 2vw, 1.65rem);
        font-weight: 650;
        letter-spacing: -0.025em;
    }

    .tool-item p {
        max-width: 70ch;
        margin: 0;
        color: var(--ink-soft);
        font-family: var(--reading);
        font-size: 0.9rem;
        line-height: 1.48;
    }

    .tool-arrow {
        transition: transform 200ms ease;
    }

    .tool-item:hover .tool-arrow {
        transform: translate(0.25rem, -0.25rem);
    }

    .archive-context {
        display: grid;
        grid-template-columns: 1fr 1fr;
        min-height: 48rem;
        color: var(--paper);
        background: var(--ink);
    }

    .context-image {
        position: relative;
        min-height: 45rem;
    }

    .context-image::after {
        position: absolute;
        inset: 0;
        content: "";
        background: linear-gradient(180deg, transparent 52%, rgba(0, 0, 0, 0.76));
    }

    .context-image img {
        width: 100%;
        height: 100%;
        object-fit: cover;
        object-position: center 34%;
        filter: saturate(0.72);
    }

    .context-copy {
        display: flex;
        flex-direction: column;
        justify-content: center;
        max-width: 43rem;
        padding: clamp(3rem, 7vw, 8rem);
    }

    .context-copy .eyebrow {
        color: var(--saffron);
    }

    .context-copy h2 {
        margin-bottom: 2rem;
        font-family: var(--serif);
        font-size: clamp(3.3rem, 6vw, 6.5rem);
        font-weight: 400;
        letter-spacing: -0.05em;
        line-height: 0.92;
    }

    .context-copy > p:not(.eyebrow) {
        color: rgba(244, 239, 229, 0.7);
        font-family: var(--reading);
        font-size: clamp(0.96rem, 1.2vw, 1.06rem);
        line-height: 1.65;
    }

    .context-copy .text-link {
        margin-top: 1.2rem;
        color: var(--paper);
    }

    @media (max-width: 1050px) {
        .hero {
            grid-template-columns: 1fr;
        }

        .hero-copy {
            min-height: 39rem;
        }

        .hero-visual {
            min-height: 48rem;
        }

        .project-intro {
            grid-template-columns: 5rem 1fr;
        }

        .project-statement {
            grid-template-columns: 1fr;
        }

        .evidence-grid {
            grid-template-columns: repeat(2, 1fr);
        }

        .evidence-grid article:nth-child(3) {
            border-left: 0;
            border-top: 1px solid var(--line);
        }

        .evidence-grid article:nth-child(4) {
            border-top: 1px solid var(--line);
        }
    }

    @media (max-width: 800px) {
        .stats {
            grid-template-columns: repeat(2, 1fr);
            row-gap: 1.5rem;
        }

        .stats > div:nth-child(3) {
            padding-left: 0;
            border-left: 0;
        }

        .featured-grid {
            grid-template-columns: repeat(2, 1fr);
        }

        .section-heading {
            grid-template-columns: 1fr;
        }

        .archive-context {
            grid-template-columns: 1fr;
        }

        .context-image {
            min-height: 36rem;
        }
    }

    @media (max-width: 620px) {
        .hero {
            min-height: auto;
        }

        .hero-copy {
            min-height: 0;
        }

        .award {
            margin-bottom: 2.5rem;
        }

        .hero-actions {
            width: 100%;
        }

        .hero-actions .button {
            width: 100%;
        }

        .scroll-cue {
            display: none;
        }

        .hero-visual {
            min-height: 34rem;
        }

        .hero-swatch {
            height: 57%;
        }

        .hero-portrait {
            width: 56%;
            height: 52%;
        }

        .project-intro {
            display: block;
        }

        .section-marker {
            display: none;
        }

        .evidence-grid {
            grid-template-columns: 1fr;
        }

        .evidence-grid article {
            min-height: 13rem;
            border-top: 1px solid var(--line);
            border-left: 0;
        }

        .evidence-grid article:first-child {
            border-top: 0;
        }

        .evidence-grid h3 {
            margin-top: 3rem;
        }

        .featured-grid {
            grid-template-columns: 1fr;
        }

        .textile-card {
            display: grid;
            grid-template-columns: 0.9fr 1.1fr;
        }

        .textile-card figure {
            aspect-ratio: 0.9;
        }

        .textile-card > div {
            display: flex;
            flex-direction: column;
            justify-content: flex-end;
            border-top: 0;
            border-left: 1px solid var(--line-strong);
        }

        .tool-item {
            grid-template-columns: 2rem 1fr 1.5rem;
            gap: 1rem;
        }

        .tool-item > :global(svg:not(.tool-arrow)) {
            display: none;
        }

        .tool-item h3 {
            font-size: 1.12rem;
        }

        .tools-heading {
            grid-template-columns: 1fr;
        }

        .tools-heading .eyebrow {
            grid-column: 1;
        }

        .context-image {
            min-height: 31rem;
        }

        .context-copy {
            padding: 4rem var(--page-pad);
        }
    }
</style>
