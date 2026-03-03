document.addEventListener("DOMContentLoaded", function () {
  const slides = document.querySelectorAll(".remark-slide-content");

  slides.forEach((slide) => {
    const link = document.createElement("a");
    link.href = "https://are25.netlify.app/#1";
    link.innerHTML = '<i class="fa-solid fa-house"></i>';
    link.style.position = "absolute";
    link.style.bottom = "10px";
    link.style.left = "10px";
    link.style.fontSize = "20px";
    link.style.zIndex = "1000";
    link.style.color = "#004c93";
    slide.appendChild(link);
  });
});
