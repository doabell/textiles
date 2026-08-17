<script lang="ts">
    import { page } from "$app/state";
    import { ArrowUpRight } from "@lucide/svelte";
    import BrandMark from "./BrandMark.svelte";

    const year = new Date().getFullYear();
    const accessedDate = new Intl.DateTimeFormat("en-US", {
        year: "numeric",
        month: "2-digit",
        day: "2-digit",
    }).format(new Date());
    const citationUrl = $derived(`https://dutchtextiletrade.org${page.url.pathname}`);
</script>

<footer class="site-footer">
    <div class="footer-lead">
        <BrandMark />
        <p>
            This project aims to understand the circulation of globally-sourced textiles on Dutch
            ships around the world in the seventeenth and eighteenth centuries by examining data
            drawn from trade records alongside samples of textiles and visual culture depicting
            textiles in use.
        </p>
    </div>

    <div class="footer-links">
        <div>
            <p class="footer-label">Explore</p>
            <a href="/textiles/">Visual Textile Glossary</a>
            <a href="/map/">Textile Geographies</a>
            <a href="/values/">Textiles, Modifiers, and Values</a>
            <a href="/swatches/">Swatch Search</a>
        </div>
        <div>
            <p class="footer-label">Project</p>
            <a href="/about/">About</a>
            <a href="/data-visualization/">Data Visualization</a>
            <a href="/data/">Our data</a>
            <a href="/sources/">Sources</a>
        </div>
        <div>
            <p class="footer-label">Connect</p>
            <a href="/contact/">Contact</a>
            <a href="https://jhna.org/issues/vol-15-1-2023/" target="_blank" rel="noreferrer">
                JHNA issue <ArrowUpRight size={13} aria-hidden="true" />
            </a>
        </div>
    </div>

    <div class="footer-bottom">
        <p>Copyright © {year} Dutch Textile Trade</p>
        <div class="citation">
            <p>Carrie Anderson and Marsely Kehoe. <em>The Dutch Textile Trade Project</em>.</p>
            <p><a href={citationUrl}>{citationUrl}</a>. Accessed {accessedDate}.</p>
        </div>
        <p>
            <a
                href="https://creativecommons.org/licenses/by-nc-sa/4.0/"
                target="_blank"
                rel="license noreferrer"
            >
                This work is licensed under a Creative Commons Attribution-NonCommercial-ShareAlike
                4.0 International License.
            </a>
        </p>
    </div>
</footer>
