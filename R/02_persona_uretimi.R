# ============================================================
# 02_persona_uretimi.R
# Simülatif birey (persona) üretimi
#
# İlke: Personalara yalnızca demografi + kısa yaşam bağlamı verilir;
# hedef gizil özelliğin düzeyi SÖYLENMEZ (aksi hâlde gizil yapıyı
# elle dikte etmiş oluruz). Bireyler arası varyans, profillerin
# çeşitliliğinden ve sicaklik = 1'den gelir.
#
# Not: Buradaki marjinal dağılımları, karşılaştıracağınız GERÇEK
# örneklemin demografik dağılımına eşleyin.
# ============================================================

persona_uret <- function(n = config$n_persona, tohum = config$tohum) {
  set.seed(tohum)

  yasam_baglami <- c(
    "yoğun bir işte çalışıyor ve iş-yaşam dengesi kurmakta zorlanıyor",
    "yakın zamanda büyük bir şehirden küçük bir şehre taşındı",
    "geniş bir arkadaş çevresi var ve sosyal etkinliklere sık katılıyor",
    "uzun süredir iş arıyor",
    "ailesiyle birlikte yaşıyor ve onlara bakım desteği veriyor",
    "yeni bir ilişkiye başladı",
    "yakın zamanda zorlu bir ayrılık yaşadı",
    "kendi işini kurmaya çalışıyor",
    "sağlık sorunlarıyla uğraşıyor",
    "sporla ve açık hava etkinlikleriyle yakından ilgileniyor",
    "sınavlara hazırlanıyor ve gelecek kaygısı taşıyor",
    "gönüllü bir toplum kuruluşunda aktif çalışıyor",
    "yeni emekli oldu ve gününü yapılandırmaya çalışıyor",
    "kalabalık bir ev ortamında kendine zaman ayırmakta zorlanıyor",
    "yurt dışında yaşayan yakınlarını özlüyor"
  )

  tibble(
    persona_id = sprintf("P%04d", seq_len(n)),
    yas        = sample(18:65, n, replace = TRUE),
    cinsiyet   = sample(c("kadın", "erkek"), n, replace = TRUE, prob = c(.55, .45)),
    egitim     = sample(c("ilköğretim", "lise", "önlisans", "lisans", "lisansüstü"),
                        n, replace = TRUE, prob = c(.10, .30, .15, .35, .10)),
    yerlesim   = sample(c("büyükşehir", "şehir", "ilçe/kasaba", "köy"),
                        n, replace = TRUE, prob = c(.45, .30, .18, .07)),
    ses        = sample(c("düşük", "orta", "yüksek"), n, replace = TRUE,
                        prob = c(.25, .60, .15)),
    baglam     = sample(yasam_baglami, n, replace = TRUE),
    # İsteğe bağlı duyarlılık koşulu: özellik düzeyi ipucu (döngüsellik riski!)
    ozellik_tohumu = if (config$ozellik_tohumu_kullan) {
      sample(c("oldukça düşük", "ortanın altında", "orta düzeyde",
               "ortanın üstünde", "oldukça yüksek"), n, replace = TRUE)
    } else NA_character_
  )
}

# Tek bir persona için LLM istemi (prompt) kurar.
# madde_sirasi: bu personaya gösterilecek seçkisiz madde sırası (madde_no vektörü)
persona_istemi_olustur <- function(p, madde_sirasi) {
  maddeler <- olcek$maddeler$metin[match(madde_sirasi, olcek$maddeler$madde_no)]

  kimlik <- paste0(
    "Aşağıda tanımlanan kişiyi canlandırıyorsun:\n",
    "- Yaş: ", p$yas, "\n",
    "- Cinsiyet: ", p$cinsiyet, "\n",
    "- Eğitim: ", p$egitim, "\n",
    "- Yaşadığı yer: ", p$yerlesim, "\n",
    "- Sosyoekonomik düzey: ", p$ses, "\n",
    "- Yaşam bağlamı: Bu kişi ", p$baglam, ".\n",
    if (!is.na(p$ozellik_tohumu)) {
      paste0("- Genel benlik değerlendirmesi: ", p$ozellik_tohumu, ".\n")
    } else ""
  )

  paste0(
    kimlik, "\n",
    "Bu kişi bir araştırma anketini dolduruyor. Soruları BU KİŞİ olarak, ",
    "onun bakış açısıyla, tutarlı ve gerçekçi biçimde yanıtla. ",
    "İdeal ya da sosyal olarak onaylanan yanıtları değil, bu kişinin gerçekte ",
    "verebileceği yanıtları ver. Gerçek insanlar her maddede aynı ucu ",
    "işaretlemez; kişinin profiline uygun doğal bir değişkenlik göster.\n\n",
    "Yönerge: ", olcek$yonerge, "\n\n",
    "Maddeler:\n",
    paste0(seq_along(maddeler), ". ", maddeler, collapse = "\n")
  )
}

personalar <- persona_uret()
write_rds(personalar, "data/islenmis/personalar.rds")
cat("Üretilen persona sayısı:", nrow(personalar), "\n")
