<script lang="ts">
    import OriginalCopy from "$lib/components/OriginalCopy.svelte";
    import { originalPageCopy } from "$lib/data/original-page-copy";
    const accessedDate = new Intl.DateTimeFormat("en-US", {
        year: "numeric",
        month: "long",
        day: "numeric",
    }).format(new Date());
    const sourceHtml = originalPageCopy.about.html
        .replaceAll("[insert date]", accessedDate)
        .replace(
            "<p><strong>State of the project: </strong>",
            '<h2 id="state">State of the project:</h2><p>',
        )
        .replace(
            "<p><strong>Overview of the website:</strong></p>",
            '<h2 id="overview">Overview of the website:</h2>',
        )
        .replace("<p><strong>Support:</strong></p>", '<h2 id="support">Support:</h2>')
        .replace(
            "<p><strong>How to Cite:&nbsp;</strong></p>",
            '<h2 id="citation">How to Cite:</h2>',
        )
        .replace(/<p><strong>(Chicago Manual Style[^<]+)<\/strong><\/p>/g, "<h3>$1</h3>")
        .replace(
            "<p><strong>Data visualizations generated from the project apps can be cited like this</strong>: </p>",
            "<h3>Data visualizations generated from the project apps can be cited like this:</h3>",
        );
</script>

<svelte:head
    ><title>About — Dutch Textile Trade Project</title><meta
        name="description"
        content="Welcome to the Dutch Textile Trade Project."
    /></svelte:head
>

<div class="page-shell"><header class="page-intro"><h1>About</h1></header></div>
<section class="about-copy page-shell">
    <nav aria-label="Page sections">
        <a href="#state">Project</a><a href="#overview">Overview</a><a href="#support">Support</a><a
            href="#citation">Citation</a
        >
    </nav>
    <OriginalCopy html={sourceHtml} />
</section>

<style>
    .about-copy {
        display: grid;
        grid-template-columns: minmax(7.5rem, 10rem) minmax(0, 1fr);
        gap: clamp(2.5rem, 6vw, 6rem);
        padding-block: clamp(3rem, 6vw, 6rem) clamp(5rem, 9vw, 8rem);
    }
    nav {
        position: sticky;
        top: 7rem;
        display: grid;
        align-self: start;
        border-top: 1px solid var(--line-strong);
    }
    nav a {
        padding: 0.8rem 0;
        border-bottom: 1px solid var(--line);
        font: var(--type-label);
        text-decoration: none;
    }
    nav a:hover {
        color: var(--madder);
    }
    @media (max-width: 700px) {
        .about-copy {
            grid-template-columns: 1fr;
            gap: 2rem;
        }
        nav {
            position: static;
            grid-template-columns: repeat(4, 1fr);
        }
    }
</style>
