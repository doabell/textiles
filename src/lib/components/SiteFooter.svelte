<script lang="ts">
    import { ArrowUpRight } from "@lucide/svelte";
    import BrandMark from "./BrandMark.svelte";

    const year = new Date().getFullYear();
    const accessedDate = new Intl.DateTimeFormat("en-US", {
        year: "numeric",
        month: "2-digit",
        day: "2-digit",
    }).format(new Date());
</script>

<footer class="site-footer">
    <div class="footer-lead">
        <a class="brand" href="/"
            ><BrandMark /><span class="brand-title">The Dutch<br />Textile Trade Project</span></a
        >
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
            <p><a href="/">www.dutchtextiletrade.org</a>. Accessed {accessedDate}.</p>
        </div>
        <div class="footer-license">
            <a
                class="license-badge"
                href="https://creativecommons.org/licenses/by-nc-sa/4.0/"
                target="_blank"
                rel="license noreferrer"
            >
                <img src="/marks/cc-by-nc-sa.svg" alt="CC BY-NC-SA" width="120" height="42" />
            </a>
            <p>
                This work is licensed under a
                <a
                    href="https://creativecommons.org/licenses/by-nc-sa/4.0/"
                    target="_blank"
                    rel="license noreferrer"
                    >Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International License</a
                >.
            </p>
        </div>
    </div>
</footer>

<style>
    .footer-license {
        display: grid;
        justify-items: start;
        gap: 0.8rem;
        max-width: 28rem;
        min-width: 0;
    }
    .license-badge {
        display: block;
    }
    .license-badge img {
        width: 120px;
        height: 42px;
    }
</style>
