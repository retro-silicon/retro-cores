package retro.sim

import java.io.{File, PrintWriter}
import java.nio.file.{Files, StandardCopyOption}
import scala.io.Source

/** A test-ROM report: a line per ROM in `out/<name><shard>.tsv` (suite, ROM, PASS / FAIL / SKIP,
  * detail) and the screen each ROM ended on in `out/frames/<name>/<ROM>.png`, with the reference
  * screenshot it was compared with, if any, beside it as `<ROM>.ref.png`. `CiReport` reads them. */
class RomReport(name: String, shard: String = "") {
  new File("out").mkdirs()
  private val log = new PrintWriter(new File(s"out/$name$shard.tsv"))
  def frame(rom: String) = { val f = new File(s"out/frames/$name/$rom.png"); f.getParentFile.mkdirs(); f }
  def reference(rom: String, png: File) = Files.copy(png.toPath, new File(s"out/frames/$name/$rom.ref.png").toPath, StandardCopyOption.REPLACE_EXISTING)
  def add(suite: String, rom: String, status: String, detail: String) = synchronized {
    log.println(Seq(suite, rom, status, detail.replaceAll("\\s+", " ").trim).mkString("\t"))
    log.flush()
  }
  def close() = log.close()
}

/** The CI's view of every report under `out/`, and of the ScalaTest results under `out/tests/`: a Markdown summary for the run page
  * (`out/summary.md`), a screenshot gallery (`out/site/index.html`), and an exit code that fails
  * only on regressions. `ci/known-failures/<report>.txt` lists the ROMs a report is known to fail;
  * a failure not listed fails the run, a listed ROM that passes is reported so the list can shrink.
  * A report without a list is shown but not judged.
  * `out/site/known-failures/` holds the lists as this run found them.
  * {{{ sbt "Test/runMain retro.sim.CiReport [gallery URL]" }}} */
object CiReport extends App {
  case class Line(report: String, suite: String, rom: String, status: String, detail: String)

  def lines(f: File) = {
    val s = Source.fromFile(f);
    try s.getLines().toList
    finally s.close()
  }
  val tsvs = Option(new File("out").listFiles()).toSeq.flatten.filter(_.getName.endsWith(".tsv")).sortBy(_.getName)
  val results = tsvs.flatMap { f =>
    val report = f.getName.stripSuffix(".tsv").replaceAll("-\\d+$", "")
    lines(f).map(_.split("\t", -1)).collect { case Array(suite, rom, status, detail) => Line(report, suite, rom, status, detail) }
  }
  val reports = results.groupBy(_.report).toSeq.sortBy(_._1)
  def known(report: String) = Some(new File(s"ci/known-failures/$report.txt")).filter(_.isFile).map(lines(_).map(_.trim).filter(_.nonEmpty).toSet)

  val site = new File("out/site")
  new File(site, "known-failures").mkdirs()
  val md = new StringBuilder
  md ++= "| Report | Pass | Fail | Skip | New failures | Now passing |\n|---|--:|--:|--:|--:|--:|\n"
  var regressions = 0
  val details = new StringBuilder
  for ((report, rs) <- reports) {
    val fails = rs.filter(_.status == "FAIL")
    val list = known(report)
    val newFails = list.map(k => fails.filterNot(r => k(r.rom))).getOrElse(Nil)
    val nowPassing = list.map(k => rs.filter(r => r.status == "PASS" && k(r.rom))).getOrElse(Nil)
    regressions += newFails.size
    def n(s: String) = rs.count(_.status == s)
    md ++= s"| $report | ${n("PASS")} | ${n("FAIL")} | ${n("SKIP")} | ${list.fold("no list")(_ => newFails.size.toString)} | ${nowPassing.size} |\n"
    for (r <- newFails) details ++= s"- **$report** new failure `${r.rom}`: ${r.detail}\n"
    for (r <- nowPassing) details ++= s"- **$report** now passes `${r.rom}`: remove it from `ci/known-failures/$report.txt`\n"
    val p = new PrintWriter(new File(site, s"known-failures/$report.txt"))
    fails.map(_.rom).sorted.foreach(p.println)
    p.close()
  }
  if (reports.isEmpty) md ++= "| (no reports) | | | | | |\n"
  // ScalaTest's JUnit XML: a row per job, from each suite's counts.
  for (job <- Option(new File("out/tests").listFiles()).toSeq.flatten.sortBy(_.getName)) {
    val counts = job.listFiles().toSeq.filter(_.getName.endsWith(".xml")).flatMap { f =>
      val head = """<testsuite [^>]*>""".r.findFirstIn(Source.fromFile(f).mkString).getOrElse("")
      def n(attr: String) = raw"""\b$attr="(\d+)"""".r.findFirstMatchIn(head).map(_.group(1).toInt).getOrElse(0)
      Seq((n("tests"), n("failures") + n("errors"), n("skipped")))
    }
    val (tests, failed, skipped) = counts.foldLeft((0, 0, 0)) { case ((a, b, c), (x, y, z)) => (a + x, b + y, c + z) }
    md ++= s"| tests: ${job.getName} | ${tests - failed - skipped} | $failed | $skipped | | |\n"
  }
  md ++= "\n" ++= details
  for (url <- args.headOption) md ++= s"\nScreenshots of every ROM: [gallery]($url)\n"

  // Gallery: a section per report (the RTL's first and open), a grid per suite of the ROMs that
  // ran, each ROM's last screen beside its reference.
  def esc(s: String) = s.replace("&", "&amp;").replace("<", "&lt;").replace(">", "&gt;").replace("\"", "&quot;")
  def count(rs: Seq[Line]) = s"${rs.count(_.status == "PASS")} pass, ${rs.count(_.status == "FAIL")} fail"
  val shown = reports.sortBy { case (report, _) => (!report.endsWith("-rtl"), report) }
  val html = new StringBuilder
  html ++= """<!doctype html><html><head><meta charset="utf-8"><meta name="viewport" content="width=device-width">
<title>retro-cores test ROMs</title><style>
:root{--bg:#fff;--fg:#222;--dim:#777;--card:#f3f3f3;--pass:#2a7;--fail:#d33}
@media (prefers-color-scheme:dark){:root{--bg:#161616;--fg:#ddd;--dim:#999;--card:#242424}}
body{background:var(--bg);color:var(--fg);font:14px system-ui,sans-serif;margin:16px}a{color:inherit}
.grid{display:flex;flex-wrap:wrap;gap:8px}.card{background:var(--card);padding:6px;border-radius:6px;border-top:4px solid var(--pass);max-width:100%}
.FAIL{border-top-color:var(--fail)}img{image-rendering:pixelated;height:144px;margin-right:2px}
.name{font-size:11px;max-width:330px;overflow-wrap:anywhere}.detail{font-size:11px;max-width:330px;color:var(--fail)}
summary{font-size:1.4em;font-weight:bold;margin:12px 0;cursor:pointer}nav{color:var(--dim);margin-bottom:4px}
body.failing .card:not(.FAIL),body.failing .ok{display:none}
</style></head><body><h1>retro-cores test ROMs</h1>
<p>The screen each test ROM ended on, beside the reference screenshot where the test compares one; green passed, red failed.
<label><input type="checkbox" onchange="document.body.classList.toggle('failing',this.checked)"> failing only</label></p>
"""
  for ((report, rs) <- shown) {
    val ran = rs.filter(_.status != "SKIP").groupBy(_.suite).toSeq.sortBy(_._1)
    html ++= s"""<details${if (report.endsWith("-rtl")) " open" else ""}><summary>$report: ${count(rs)}, ${rs.count(
        _.status == "SKIP")} skipped</summary>\n<nav>"""
    html ++= ran.map { case (suite, ss) => s"""<a href="#$report-${esc(suite)}">${esc(suite)}</a> ${ss.count(_.status == "PASS")}/${ss.size}""" }.mkString(" · ")
    html ++= "</nav>\n"
    for ((suite, ss) <- ran) {
      val ok = if (ss.forall(_.status == "PASS")) " class=ok" else ""
      html ++= s"""<h3 id="$report-${esc(suite)}"$ok>${esc(suite)}: ${count(ss)}</h3><div class=grid>\n"""
      for (r <- ss.sortBy(_.rom)) {
        def img(suffix: String, alt: String) = {
          val f = new File(s"out/frames/$report/${r.rom}$suffix")
          if (!f.isFile) ""
          else {
            val to = new File(site, s"frames/$report/${r.rom}$suffix")
            to.getParentFile.mkdirs()
            Files.copy(f.toPath, to.toPath, StandardCopyOption.REPLACE_EXISTING)
            s"""<img loading=lazy alt=$alt src="${esc(s"frames/$report/${r.rom}$suffix".replace("#", "%23").replace(" ", "%20"))}">"""
          }
        }
        val detail = if (r.status == "FAIL") s"<div class=detail>${esc(r.detail)}</div>" else ""
        html ++= s"""<div class="card ${r.status}" title="${esc(r.detail)}">${img(".png", "screen")}${img(".ref.png", "reference")}"""
        html ++= s"""<div class=name>${esc(r.rom.dropWhile(_ != '/').drop(1))}</div>$detail</div>\n"""
      }
      html ++= "</div>\n"
    }
    html ++= "</details>\n"
  }
  html ++= "</body></html>\n"
  def write(f: File, s: String) = { val p = new PrintWriter(f); p.print(s); p.close() }
  write(new File(site, "index.html"), html.toString)
  write(new File("out/summary.md"), md.toString)
  println(md)
  if (regressions > 0) sys.exit(1)
}
