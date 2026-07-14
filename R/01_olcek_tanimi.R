# ============================================================
# 01_olcek_tanimi.R
# Ölçek ve madde havuzu tanımı
#
# Örnek olarak Rosenberg Benlik Saygısı Ölçeği (10 madde,
# 4'lü Likert, 5 ters madde) kullanılmıştır. Kendi ölçeğinizi
# incelemek için yalnızca bu dosyayı ve config'deki Likert
# aralığını değiştirmeniz yeterlidir.
# ============================================================

olcek <- list(
  ad        = "Rosenberg Benlik Saygısı Ölçeği",
  yonerge   = paste0(
    "Aşağıdaki ifadelerin her birine ne ölçüde katıldığınızı belirtiniz. ",
    "1 = Kesinlikle katılmıyorum, 2 = Katılmıyorum, ",
    "3 = Katılıyorum, 4 = Kesinlikle katılıyorum."
  ),
  maddeler = tribble(
    ~madde_no, ~metin,                                                              ~alt_boyut, ~ters,
    1,  "Kendimi en az diğer insanlar kadar değerli buluyorum.",                    "genel",    FALSE,
    2,  "Bazı olumlu özelliklerimin olduğunu düşünüyorum.",                         "genel",    FALSE,
    3,  "Genelde kendimi başarısız bir kişi olarak görme eğilimindeyim.",           "genel",    TRUE,
    4,  "Ben de diğer insanların birçoğunun yapabildiği kadar bir şeyler yapabilirim.", "genel", FALSE,
    5,  "Kendimde gurur duyacak fazla bir şey bulamıyorum.",                        "genel",    TRUE,
    6,  "Kendime karşı olumlu bir tutum içindeyim.",                                "genel",    FALSE,
    7,  "Genel olarak kendimden memnunum.",                                         "genel",    FALSE,
    8,  "Kendime karşı daha fazla saygı duyabilmeyi isterdim.",                     "genel",    TRUE,
    9,  "Bazen kesinlikle kendimin bir işe yaramadığını düşünüyorum.",              "genel",    TRUE,
    10, "Bazen kendimin hiç de yeterli bir insan olmadığını düşünüyorum.",          "genel",    TRUE
  )
)

n_madde       <- nrow(olcek$maddeler)
madde_adlari  <- paste0("m", olcek$maddeler$madde_no)
ters_maddeler <- madde_adlari[olcek$maddeler$ters]

# DFA (lavaan) için ölçme modeli — kuramsal yapı burada tanımlanır.
# Tek faktörlü örnek; çok boyutlu ölçekte alt_boyut sütunundan üretin.
dfa_modeli <- paste0("F1 =~ ", paste(madde_adlari, collapse = " + "))
